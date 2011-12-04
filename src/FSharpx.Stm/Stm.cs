/*
 * Copyright (c) 2008, Gregory Neverov
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
 * 1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution. 
 * 3. Neither the name of the author nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission. 
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 * 
 */
using System;
using System.Collections.Generic;
using System.Threading;

namespace FSharpx.Stm.Core
{
	public abstract class TVar : IComparable<TVar>
	{
		private static int nextId = 0;

		private int id;

		internal TVar()
		{
			this.id = Interlocked.Increment(ref nextId);
		}

		public int CompareTo(TVar other)
		{
			return id.CompareTo(other.id);
		}
	}

	public sealed class TVar<T> : TVar
	{
		internal T value;
		internal IEqualityComparer<T> cmp;

		internal TVar(T value, IEqualityComparer<T> cmp)
		{
			this.value = value;
			this.cmp = cmp;
		}

		public T UnsafeRead()
		{
			return value;
		}
	}

	public sealed class TLog
	{
		private abstract class Entry
		{
			public abstract TVar Location { get; }
			public abstract bool IsValid();
			public abstract void Commit();
			public abstract void MergeNested(Entry entry);
		}

		private sealed class Entry<T> : Entry
		{
			public TVar<T> location;
			public bool hasOldValue;
			public T oldValue;
			public T newValue;

			// read
			public Entry(TVar<T> location)
			{
				this.location = location;
				this.hasOldValue = true;
				
				this.oldValue = location.value;
				this.newValue = this.oldValue;
			}

			// write
			public Entry(TVar<T> location, T value)
			{
				this.location = location;
				this.hasOldValue = false;
				this.oldValue = default(T);
				this.newValue = value;
			}

			public override TVar Location
			{
				get { return location; }
			}

			public override void Commit()
			{
				location.value = newValue;
			}

			public override void MergeNested(Entry entry)
			{
				((Entry<T>)entry).newValue = this.newValue;
			}

			public override bool IsValid()
			{
				return !hasOldValue || location.cmp.Equals(location.value, oldValue);
			}
		}

		private sealed class ReferenceEqualityComparer<T> : IEqualityComparer<T> where T : class
		{
			public bool Equals(T x, T y)
			{
				return x == y;
			}

			public int GetHashCode(T obj)
			{
				return obj.GetHashCode();
			}
		}

		private sealed class EquatableEqualityComparer<T> : IEqualityComparer<T> where T : IEquatable<T>
		{
			public bool Equals(T x, T y)
			{
				return x.Equals(y);
			}

			public int GetHashCode(T obj)
			{
				return obj.GetHashCode();
			}
		}

		private sealed class AnyEqualityComparer<T> : IEqualityComparer<T>
		{
			public bool Equals(T x, T y)
			{
				return x.Equals(y);
			}

			public int GetHashCode(T obj)
			{
				return obj.GetHashCode();
			}
		}

		private TLog outer = null;
		private SortedDictionary<TVar, Entry> log = new SortedDictionary<TVar, Entry>();

		public static TVar<T> NewTVarClass<T>(T value) where T : class
		{
			return new TVar<T>(value, new ReferenceEqualityComparer<T>());
		}

		public static TVar<T> NewTVarStruct<T>(T value) where T : struct, IEquatable<T>
		{
			return new TVar<T>(value, new EquatableEqualityComparer<T>());
		}

		public static TVar<T> NewTVarBoxedStruct<T>(T value) where T : struct
		{
			return new TVar<T>(value, new AnyEqualityComparer<T>());
		}

		public static TVar<T> NewTVar<T>(T value)
		{
			Type t = typeof(T);
			Type ect;
			if (!t.IsValueType) ect = typeof(ReferenceEqualityComparer<>);
			else if (typeof(IEquatable<T>).IsAssignableFrom(t)) ect = typeof(EquatableEqualityComparer<>);
			else ect = typeof(AnyEqualityComparer<>);
			IEqualityComparer<T> eci = (IEqualityComparer<T>)Activator.CreateInstance(ect.MakeGenericType(t));
			return new TVar<T>(value, eci);
		}

		public T ReadTVar<T>(TVar<T> location)
		{
			TLog trans = this;
			do
			{
				Entry entry;
				if(trans.log.TryGetValue(location, out entry))
					return ((Entry<T>) entry).newValue;
				trans = trans.outer;
			} while(trans != null);

			Entry<T> entryT = new Entry<T>(location);
			log.Add(location, entryT);
			return entryT.oldValue;
		}

		public void WriteTVar<T>(TVar<T> location, T value)
		{
			Entry entry;
			if(log.TryGetValue(location, out entry)) {
				((Entry<T>) entry).newValue = value;
			}
			else {
				Entry<T> entryT = new Entry<T>(location, value);
				log.Add(location, entryT);
			}
		}

		//requires lock
		internal bool IsValid()
		{
			return IsValidSingle() && (outer == null || outer.IsValid());
		}

		//requires lock
		private bool IsValidSingle()
		{
			foreach (Entry entry in log.Values)
				if (!entry.IsValid()) return false;
			return true;
		}

		//requires lock
		internal void Commit()
		{
			if (outer != null) throw new InvalidOperationException();
			foreach (Entry entry in log.Values) entry.Commit();
		}

		internal void Wait()
		{
		}

		internal void UnWait()
		{
		}

		internal TLog StartNested()
		{
			TLog inner = new TLog();
			inner.outer = this;
			return inner;
		}

		internal void MergeNested()
		{
			foreach (Entry innerEntry in log.Values)
			{
				Entry outerEntry;
				if (outer.log.TryGetValue(innerEntry.Location, out outerEntry))
					innerEntry.MergeNested(outerEntry);
				else
					outer.log.Add(innerEntry.Location, innerEntry);
			}
		}

		private static object @lock = new object();

		private void Lock()
		{
			Monitor.Enter(@lock);
			//Console.WriteLine("Thread {0} acquired lock.", Thread.CurrentThread.Name);
		}

		private void UnLock()
		{
			//Console.WriteLine("Thread {0} released lock.", Thread.CurrentThread.Name);
			Monitor.Exit(@lock);
		}

		private void Block()
		{
			//Console.WriteLine("Thread {0} entered wait.", Thread.CurrentThread.Name);
			Monitor.Wait(@lock);
			//Console.WriteLine("Thread {0} exited wait.", Thread.CurrentThread.Name);
		}

		private void Signal()
		{
			Monitor.PulseAll(@lock);
			//Console.WriteLine("Thread {0} pulsed.", Thread.CurrentThread.Name);
		}

		public static void Atomic(StmAction p)
		{
			Atomic<object>(Ignore(p));
		}

		public static T Atomic<T>(StmAction<T> p)
		{
			TLog trans = new TLog();
			while (true)
			{
				bool retry = false;
				try
				{
					T result = p(trans);
					trans.Lock();
					bool isValid = trans.IsValid();
					if (isValid)
					{
						trans.Commit();
						trans.Signal();
					}
					trans.UnLock();
					if (isValid) return result;
				}
				catch (RetryException)
				{
					// cannot receive ThreadInterruptedException in catch handler
					retry = true;
				}
				catch (CommitFailedException)
				{
				}
				catch (ThreadInterruptedException)
				{
					throw;
				}
				catch
				{
					trans.Lock();
					bool isValid = trans.IsValid();
					trans.UnLock();
					if (isValid) throw;
				}
				if (retry)
				{
					trans.Lock();
					bool isValid = trans.IsValid();
					if (isValid)
					{
						trans.Wait();
						try
						{
							do { trans.Block(); }
							while (trans.IsValid());
						}
						finally
						{
							trans.UnWait();
							trans.UnLock();
						}
					}
					else trans.UnLock();
				}
				trans.log.Clear();
				Thread.Sleep(0);
			}
		}

		public void Retry()
		{
			throw new RetryException();
		}

		public T Retry<T>()
		{
			throw new RetryException();
		}

		public void OrElse(StmAction p, StmAction q)
		{
			OrElse<object>(Ignore(p), Ignore(q));
		}

		public T OrElse<T>(StmAction<T> p, StmAction<T> q)
		{
			TLog fst = this.StartNested();
			try
			{
				T result = p(fst);
				fst.Lock();
				bool isValid = fst.IsValid();
				fst.UnLock();
				if (isValid) { fst.MergeNested(); return result; }
				else throw new CommitFailedException();
			}
			catch (RetryException)
			{
				TLog snd = this.StartNested();
				try
				{
					T result = q(snd);
					snd.Lock();
					bool isValid = snd.IsValid();
					snd.UnLock();
					if (isValid) { snd.MergeNested(); return result; }
					else throw new CommitFailedException();
				}
				catch (RetryException)
				{
					this.Lock();
					bool isValid = fst.IsValidSingle() && snd.IsValidSingle() && this.IsValid();
					this.UnLock();
					if (isValid)
					{
						fst.MergeNested();
						snd.MergeNested();
						throw;
					}
					else throw new CommitFailedException();
				}
				catch (CommitFailedException)
				{
					throw;
				}
				catch (ThreadInterruptedException)
				{
					throw;
				}
				catch
				{
					snd.Lock();
					bool isValid = snd.IsValid();
					snd.UnLock();
					if (isValid) { snd.MergeNested(); throw; }
					else throw new CommitFailedException();
				}
			}
			catch (CommitFailedException)
			{
				throw;
			}
			catch (ThreadInterruptedException)
			{
				throw;
			}
			catch
			{
				fst.Lock();
				bool isValid = fst.IsValid();
				fst.UnLock();
				if (isValid) { fst.MergeNested(); throw; }
				else throw new CommitFailedException();
			}
		}

		public static StmAction<object> Ignore(StmAction p)
		{
			return delegate(TLog trans) { p(trans); return null; };
		}

		private sealed class RetryException : Exception
		{
		}

		private sealed class CommitFailedException : Exception
		{
		}
	}

	public delegate void StmAction(TLog trans);

	public delegate T StmAction<T>(TLog trans);
}
