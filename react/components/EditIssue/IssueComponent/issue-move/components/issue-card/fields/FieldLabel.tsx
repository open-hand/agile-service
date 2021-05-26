import React, { useCallback } from 'react';
import { Select } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import TextEditToggle from '@/components/TextEditTogglePro';
import LabelTags from '@/components/tag/label-tags';
import SelectLabel from '@/components/select/select-label';
import { FieldCommonProps } from '../Field';

export interface FieldLabelProps extends FieldCommonProps {

}
const FieldLabel: React.FC<FieldLabelProps> = ({ target, onChange, fieldWithValue }) => {
  const handleChange = useCallback((value) => {
    onChange && onChange(value, value);
  }, [onChange]);
  return (
    <TextEditToggle
      onSubmit={handleChange}
      initValue={fieldWithValue?.value}
      alwaysRender={false}
      submitTrigger={['blur']}
      editor={() => (
        <SelectLabel multiple projectId={target.projectId} combo={false} primitiveValue={false} />
      )}
    >
      <LabelTags
        data={fieldWithValue?.valueStr}
      />
    </TextEditToggle>
  );
};

export default observer(FieldLabel);
