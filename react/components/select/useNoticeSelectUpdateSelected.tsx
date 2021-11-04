import { useCounter, useCreation, usePersistFn } from 'ahooks';

/**
 *  当值更新时给予通知
 * @returns
 */
export function useNoticeSelectUpdateSelected(): [any, ((key: string, val?: string) => void)] {
  const [forceUpdateValue, { inc: notice }] = useCounter(0);
  const values = useCreation(() => new Map<string, { old?: any; }>(), []);
  const setValue = usePersistFn((key: string, val?: string) => {
    const { old } = values.get(key) || {};
    if (old !== val) {
      values.set(key, { old: val });
      notice();
    } else {
      notice(0);
    }
  });
  return [forceUpdateValue, setValue];
}
