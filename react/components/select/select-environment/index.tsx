import React, {
  useMemo, forwardRef, useEffect, useState,
} from 'react';
import { Select as OldSelect } from 'choerodon-ui';

import { Select } from 'choerodon-ui/pro';
import { SelectProps as OldSelectProps } from 'choerodon-ui/lib/select';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import FlatSelect from '@/components/flat-select';
import { commonApi } from '@/api';

interface Props extends Partial<SelectProps> {
  flat?: boolean
}
function useEnvironmentData() {
  const [options, setOptions] = useState<{ valueCode: string, name: string }[]>([]);
  useEffect(() => {
    commonApi.loadLookupValue('environment').then((res: any) => {
      const { lookupValues = [] } = res || {};
      setOptions(lookupValues);
    });
  }, []);
  return options;
}
const SelectEnvironment: React.FC<Props> = forwardRef(({
  flat, ...otherProps
}, ref: React.Ref<Select>) => {
  const Component = flat ? FlatSelect : Select;
  const options = useEnvironmentData();
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
const OldSelectEnvironment: React.FC<Partial<OldSelectProps>> = (props) => {
  const options = useEnvironmentData();
  return (
    <OldSelect
      allowClear
      {...props}
    >
      {options.map((option) => <OldSelect.Option value={option.valueCode}>{option.name}</OldSelect.Option>)}
    </OldSelect>
  );
};
export default SelectEnvironment;
export { OldSelectEnvironment };
