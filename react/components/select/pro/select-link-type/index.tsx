import React, { forwardRef } from 'react';
import { Select } from 'choerodon-ui/pro';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import { FlatSelect } from '@choerodon/components';
import useLinkType from '@/hooks/data/useLinkType';

interface Props extends Partial<SelectProps> {
  flat?: boolean
  projectId?: string
}
const { Option } = Select;
const SelectLinkType: React.FC<Props> = forwardRef(({
  flat, projectId, ...otherProps
},
ref: React.Ref<Select>) => {
  const { data } = useLinkType({ projectId });
  const Component = flat ? FlatSelect : Select;
  return (
    <Component
      ref={ref}
      clearButton={false}
      {...otherProps}
    >
      {data?.map((link) => (
        <Option value={`${link.linkTypeId}+${link.isIn}`}>
          {link.name}
        </Option>
      ))}
    </Component>
  );
});
export default SelectLinkType;
