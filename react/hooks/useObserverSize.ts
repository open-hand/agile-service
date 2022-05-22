import { useLayoutEffect, useRef } from 'react';
import { useSafeState } from 'ahooks';
import { getTargetElement } from '@/utils/dom';
import type { BasicTarget, TargetElement } from '@/utils/dom';

interface Size {
    width?: number;
    height?: number;
}
export interface IHookObserverSizeOptions {
    /**
     * 更新size时
     * @returns `false` 阻止更新
     */
    updateSize?: (preSize: Size, nextSize: Size) => void | boolean
}
function defaultObserverUpdateSize() {
  return true;
}
/**
 * 可控的dom节点 `height` `weight` 获取hook
 * @param target
 * @param options
 * @returns
 */
function useObserverSize(target?: BasicTarget<TargetElement>, options?: IHookObserverSizeOptions) {
  const updateSizeRef = useRef<NonNullable<IHookObserverSizeOptions['updateSize']>>();
  updateSizeRef.current = options?.updateSize || defaultObserverUpdateSize;
  const [size, setSize] = useSafeState<Size>();
  useLayoutEffect(() => {
    const el = getTargetElement(target);
    if (!el) {
      return () => { };
    }
    const preSize = {} as Size;
    const resizeObserver = new ResizeObserver((entries:any[]) => {
      entries.forEach((entry:any) => {
        const nextSize = {
          width: entry.target.clientWidth,
          height: entry.target.clientHeight,
        };
        const updated = updateSizeRef.current && updateSizeRef.current(preSize, nextSize);
        updated && setSize(nextSize);
      });
    });
    resizeObserver.observe(el as HTMLElement);
    return () => {
      resizeObserver.disconnect();
    };
  }, [setSize, target]);
  return size;
}
export default useObserverSize;
