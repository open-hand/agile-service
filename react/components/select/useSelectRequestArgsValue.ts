import {
  useCounter,
  useCreation, usePersistFn, usePrevious, useSafeState,
} from 'ahooks';
import {
  castArray, difference, get, values,
} from 'lodash';
import React, {
  MutableRefObject, useEffect, useMemo, useRef, useState,
} from 'react';
import useDeepCompareCreation from '@/hooks/useDeepCompareCreation';
import useDeepCompareEffect from '@/hooks/useDeepCompareEffect';

function valueTypeSync(targetValueType: 'string' | 'array', value?: any[]): string[] | string {
  if (value === undefined) {
    return [];
  }
  if (targetValueType === 'string') {
    return value.join(',');
  }
  if (targetValueType === 'array') {
    return value;
  }
  return value;
}

function getSelectIds(value: string[], options: any[], dataValueKey: string, preValue?: string[]) {
  if (!value.length || !options.length) {
    return value;
  }
  const differenceValues = preValue?.length ? difference(value, preValue) : value;
  if (differenceValues?.length) {
    const dataOptionValues = options.map((option) => get(option, dataValueKey));
    return differenceValues.some((v) => !dataOptionValues.includes(v)) ? value : preValue || value;
  }
  return preValue || value;
}
interface IHookSelectRequestArgsSelectedOptions {
  /**
   * 数据列表引用
   *
   * 默认取 key 为`value`
   * 这里的 `dataRef` 最终传入 `useSelect`
   * @example
   * import { priorityApi } from '@/api'
   * const dataRef = useRef();
   * const values = ['2'];
   * const selected = useSelectRequestArgsValue({ dataRef, value : values });
   * const config = useSelect({ dataRef ,request:() => priorityApi.loadByProject(), paging:false});
   */
  dataRef: MutableRefObject<((data: any[]) => void) | undefined>
  /**
   * @default 'value'
   */
  dataValueKey?: string
  /** 当前选中的值 */
  value?: string[] | string
  /**
   * 返回的selected 是否为数组形式
   * @default true
   */
  returnSelectArray?: boolean
}
/**
 * useSelect的辅助hook判断选中的值是否在列表中
 * @param options
 */
function useSelectRequestArgsValue(options: IHookSelectRequestArgsSelectedOptions & { returnSelectArray: false }): string | string
function useSelectRequestArgsValue(options: IHookSelectRequestArgsSelectedOptions & { returnSelectArray?: true }): string[]
function useSelectRequestArgsValue(options: IHookSelectRequestArgsSelectedOptions) {
  const {
    returnSelectArray = true, value, dataRef, dataValueKey = 'value',
  } = options;
  const latestValueRef = useRef<string[]>();
  const latestDataRef = useRef<any[]>();
  const ids = useCreation(() => ({ current: [] as string[] }), []);
  const valueArr = useMemo(() => castArray(value).filter(Boolean), [value]);
  latestValueRef.current = valueArr;
  const [, { inc: update }] = useCounter();
  const loadData = usePersistFn((d: any[]) => {
    latestDataRef.current = d;
    const newIds = getSelectIds(latestValueRef.current || [], d, dataValueKey, ids.current);
    const isUpdate = (newIds.length || ids.current?.length) && !Object.is(newIds, ids.current);
    if (isUpdate) {
      ids.current = newIds;
      update();
    }
  });
  if (!dataRef.current) {
    dataRef.current = loadData;
  }
  useDeepCompareEffect(() => {
    ids.current = getSelectIds(valueArr, latestDataRef.current || [], dataValueKey, ids.current);
  }, [dataValueKey, valueArr]);

  return returnSelectArray || Array.isArray(ids.current) ? ids.current : valueTypeSync(typeof value as 'string', ids.current);
}

export default useSelectRequestArgsValue;
