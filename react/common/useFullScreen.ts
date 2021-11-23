import { useFullscreen as useFull, usePersistFn } from 'ahooks';
import type { BasicTarget } from 'ahooks/lib/utils/dom';

export default function useFullScreen(target: BasicTarget, onFullScreenChange: any, customClassName = 'fullScreen') {
  const element = typeof target === 'function' ? target() : target;

  const handleExitFull = usePersistFn(() => {
    if (element && customClassName) {
      const classList = 'current' in element ? element.current?.classList : element.classList;
      classList?.remove(customClassName);
    }
  });
  const handleFull = usePersistFn(() => {
    if (element && customClassName) {
      const classList = 'current' in element ? element.current?.classList : element.classList;
      classList?.add(customClassName);
    }
  });
  const [isFullscreen, { toggleFull }] = useFull(target, {
    onFull: handleFull,
    onExitFull: handleExitFull,
  });

  return [isFullscreen, toggleFull] as const;
}
