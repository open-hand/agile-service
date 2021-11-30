import { useCounter, useCreation, usePersistFn } from 'ahooks';

function transformVal(v: any) {
  return v === '' || !v ? undefined : v;
}
/**
 *  当值更新时给予通知
 * @returns
 */

export function useNoticeSelectUpdateSelected() {
  const [forceUpdateValue, { inc: notice, set }] = useCounter(0);
  const values = useCreation(() => new Map<string, { old?: any; }>(), []);
  const setValue = usePersistFn((key: string, val?: string) => {
    const { old } = values.get(key) || {};
    const newVal = transformVal(val);
    if (old !== newVal) {
      values.set(key, { old: newVal });
      notice();
    } else {
      set(0);
    }
  });
  return [forceUpdateValue, setValue] as const;
}
