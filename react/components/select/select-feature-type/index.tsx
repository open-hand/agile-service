import React, { forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';

export interface SelectFeatureTypeProps extends Partial<SelectProps> {
  flat?: boolean
}

const SelectFeatureType: React.FC<SelectFeatureTypeProps> = forwardRef(({
  flat, ...otherProps
}, ref: React.Ref<Select>) => {
  const Component = flat ? FlatSelect : Select;

  return (
    <Component
      ref={ref}
      clearButton
      {...otherProps}
    >
      <Component.Option value="enabler">使能</Component.Option>
      <Component.Option value="business">特性</Component.Option>
    </Component>
  );
});
export default SelectFeatureType;
