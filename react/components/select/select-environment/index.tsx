import React, {
  forwardRef, useEffect, useState,
} from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { usePersistFn } from 'ahooks';
import { FlatSelect } from '@choerodon/components';
import { commonApi } from '@/api';

export interface SelectEnvironmentProps extends Partial<SelectProps> {
  flat?: boolean
  dataRef?: React.MutableRefObject<any>
  afterLoad?: (data: any[]) => void
}
interface UseEnvironmentDataProps {
  afterLoad?: SelectEnvironmentProps['afterLoad']
  dataRef?: React.MutableRefObject<any>

}
function useEnvironmentData(props?: UseEnvironmentDataProps) {
  const [options, setOptions] = useState<{ valueCode: string, name: string }[]>([]);
  const callback = usePersistFn(props?.afterLoad || (() => { }));
  useEffect(() => {
    commonApi.loadLookupValue('environment').then((res: any) => {
      const { lookupValues = [] } = res || {};
      setOptions(lookupValues);
      callback(lookupValues);
      if (props?.dataRef) {
        Object.assign(props?.dataRef, {
          current: lookupValues,
        });
      }
    });
  }, [callback, props?.dataRef]);
  return options;
}
const SelectEnvironment: React.FC<SelectEnvironmentProps> = forwardRef(({
  flat, afterLoad, dataRef, ...otherProps
}, ref: React.Ref<Select>) => {
  const Component = flat ? FlatSelect : Select;
  const options = useEnvironmentData({ afterLoad, dataRef });
  return (
    <Component
      ref={ref}
      clearButton
      {...otherProps}
    >
      {options.map((option) => <Component.Option value={option.valueCode}>{option.name}</Component.Option>)}
    </Component>
  );
});

export default SelectEnvironment;
