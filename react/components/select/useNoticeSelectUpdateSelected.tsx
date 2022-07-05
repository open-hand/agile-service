import {
  useCounter, useCreation, usePersistFn, useUpdate,
} from 'ahooks';
import { isEqual } from 'lodash';

function transformVal(v: any) {
  return v === '' || !v || (Array.isArray(v) && v.length === 0) ? undefined : v;
}
/**
 *  当值更新时给予通知
 *
 * @returns
 */

export function useNoticeSelectUpdateSelected() {
  // const [forceUpdateValue, { inc: notice, set }] = useCounter(0);
  const forceUpdate = useUpdate();
  const values = useCreation(() => new Map<string, { old?: any; }>(), []);
  const setValue = usePersistFn((key: string, val?: any, callback?: (isUpdate: boolean) => void) => {
    const { old } = values.get(key) || {};
    const newVal = transformVal(val);
    if (!isEqual(old, newVal)) {
      values.set(key, { old: newVal });
      callback && callback(true);
      forceUpdate();
      return;
    }
    callback && callback(false);
  });
  return [forceUpdate, setValue] as const;
}
