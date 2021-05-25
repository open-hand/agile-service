import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectComponent from '@/components/select/select-component';
import ComponentTags from '@/components/tag/component-tags';
import { FieldCommonProps } from '../Field';

export interface FieldComponentProps extends FieldCommonProps {

}

const FieldComponent: React.FC<FieldComponentProps> = ({ target, onChange, fieldWithValue }) => {
  const handleChange = useCallback((value) => {
    onChange && onChange(value, value);
  }, [onChange]);
  return (
    <TextEditToggle
      onSubmit={handleChange}
      initValue={fieldWithValue?.valueStr}
      mountRenderEditor={false}
      submitTrigger={['blur']}
      editor={() => (
        <SelectComponent multiple projectId={target.projectId} primitiveValue={false} />
      )}
    >
      <ComponentTags
        data={fieldWithValue?.valueStr}
      />
    </TextEditToggle>
  );
};

export default observer(FieldComponent);
