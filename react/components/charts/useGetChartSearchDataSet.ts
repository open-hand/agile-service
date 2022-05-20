import { DataSet, useDataSet } from 'choerodon-ui/pro';
import { toJS } from 'mobx';
import { DataSetProps } from 'choerodon-ui/dataset/data-set/interface';
import { useEffect, useMemo } from 'react';
import { get, isEqual } from 'lodash';
import { useDeepCompareEffectNoCheck } from '@/hooks/useDeepCompareEffect';
import isEqualNonNullable from '@/utils/isEqualNonNullable';

interface IHookGetChartSearchDataSetConfig<T extends boolean> {
    fields: NonNullable<DataSetProps['fields']>
    enabled?: T
    valueChangeDataSetValue?: { [fieldName: string]: any }
}

function useGetChartSearchDataSet<T extends boolean = true>(config: IHookGetChartSearchDataSetConfig<T>): T extends true ? DataSet : undefined {
  const dataSet = useDataSet(() => ({
    autoCreate: true,
    fields: config.fields,
  }), []);
  const fieldNames = useMemo(() => config.fields.map((field) => field.name), [config.fields]);
  useDeepCompareEffectNoCheck(() => {
    if (config.valueChangeDataSetValue !== undefined) {
      fieldNames.forEach((name) => {
        const path = name!;
        const newValue = get(config.valueChangeDataSetValue, path);
        dataSet.current?.init(path, newValue);
      });
    }
  }, [config.valueChangeDataSetValue]);
  return (config.enabled ? dataSet : undefined) as T extends true ? DataSet : undefined;
}
export default useGetChartSearchDataSet;
