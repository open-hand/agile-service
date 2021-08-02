import React, {
  forwardRef, useEffect, useState,
} from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { usePersistFn } from 'ahooks';
import { FlatSelect } from '@choerodon/components';
import { commonApi } from '@/api';

interface Props extends Partial<SelectProps> {
  flat?: boolean
  afterLoad?: (data: any[]) => void
}
interface UseEnvironmentDataProps {
  afterLoad?: Props['afterLoad']
}
function useEnvironmentData(props?: UseEnvironmentDataProps) {
  const [options, setOptions] = useState<{ valueCode: string, name: string }[]>([]);
  const callback = usePersistFn(props?.afterLoad || (() => { }));
  useEffect(() => {
    commonApi.loadLookupValue('environment').then((res: any) => {
      const { lookupValues = [] } = res || {};
      setOptions(lookupValues);
      callback(lookupValues);
    });
  }, [callback]);
  return options;
}
const SelectEnvironment: React.FC<Props> = forwardRef(({
  flat, afterLoad, ...otherProps
}, ref: React.Ref<Select>) => {
  const Component = flat ? FlatSelect : Select;
  const options = useEnvironmentData({ afterLoad });
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
