/**
 * 多次重复请求时，只取最后一次的结果
 * @param promise
 */
const makeCancelable = <T>(promise: Promise<T>): CancelAblePromise<T> => {
  let canceled = false;
  const wrappedPromise: Promise<T> = new Promise((resolve, reject) => {
    promise.then((val) => (canceled ? reject(new Error('canceled')) : resolve(val)));
    promise.catch((error) => (canceled ? reject(new Error('canceled')) : reject(error)));
  });
  const cancel = () => {
    canceled = true;
  };
  return Object.assign(wrappedPromise, { cancel });
};
interface CancelAblePromise<T> extends Promise<T> {
  cancel: Function
}

// eslint-disable-next-line max-len
export default function takeLast<T extends any[], R>(fn: (...args: T) => Promise<R>, context?: Object): (...args: T) => CancelAblePromise<R> {
  let pre: CancelAblePromise<R>;
  // eslint-disable-next-line func-names
  return function (...args: T): CancelAblePromise<R> {
    if (pre) {
      pre.cancel();
    }
    pre = makeCancelable<R>(fn.apply(context || this, args));
    return pre;
  };
}
