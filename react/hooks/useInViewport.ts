import { useEffect, useState, useMemo } from 'react';
import 'intersection-observer';
import { getTargetElement, BasicTarget } from '@/utils/dom';

type InViewport = boolean | undefined;

function isInViewPort(el: HTMLElement): InViewport {
  if (!el) {
    return undefined;
  }

  const viewPortWidth = window.innerWidth || document.documentElement.clientWidth || document.body.clientWidth;
  const viewPortHeight = window.innerHeight || document.documentElement.clientHeight || document.body.clientHeight;
  const rect = el.getBoundingClientRect();

  if (rect) {
    const {
      top, bottom, left, right,
    } = rect;
    return bottom > 0 && top <= viewPortHeight && left <= viewPortWidth && right > 0;
  }

  return false;
}

const setInViewport = (map: Map<string, InViewport>, key: string, inViewport: InViewport) => {
  map.set(key, inViewport);
};

function useInViewport(targets: Map<string, BasicTarget>): Map<string, InViewport> {
  const inViewPortMap = useMemo(() => new Map(), []);
  const [updateCount, setUpdateCount] = useState<number>(0);

  useEffect(() => {
    for (const [key, target] of targets.entries()) {
      const el = getTargetElement(target);
      inViewPortMap.set(key, isInViewPort(el as HTMLElement));
    }
    setUpdateCount((count) => count + 1);
  }, [inViewPortMap, targets]);

  useEffect(() => {
    const observer = new IntersectionObserver((entries) => {
      for (const entry of entries) {
        if (entry.isIntersecting) {
          setInViewport(inViewPortMap, entry.target.className.split('-')[1], true);
        } else {
          setInViewport(inViewPortMap, entry.target.className.split('-')[1], false);
        }
        setUpdateCount((count) => count + 1);
      }
    });

    for (const el of targets.values()) {
      observer.observe(el as Element);
    }

    return () => {
      observer.disconnect();
    };
  }, [inViewPortMap, targets]);

  return inViewPortMap;
}

export default useInViewport;
