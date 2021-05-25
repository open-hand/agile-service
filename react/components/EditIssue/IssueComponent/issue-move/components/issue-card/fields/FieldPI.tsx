import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectPI from '@/components/select/select-pi';
import PiTag from '@/components/tag/pi-tag';
import { FieldCommonProps } from '../Field';

export interface FieldPIProps extends FieldCommonProps {

}

const FieldPI: React.FC<FieldPIProps> = ({ target, onChange, fieldWithValue }) => {
  const handleChange = useCallback((value) => {
    onChange && onChange(value.piId, value);
  }, [onChange]);
  return (
    <TextEditToggle
      onSubmit={handleChange}
      initValue={fieldWithValue?.valueStr}
      mountRenderEditor={false}
      submitTrigger={['blur']}
      editor={() => (
        <SelectPI
          multiple
          projectId={target.projectId}
          statusList={['doing, todo']}
          primitiveValue={false}
        />
      )}
    >
      <PiTag
        data={fieldWithValue?.valueStr}
      />
    </TextEditToggle>
  );
};

export default observer(FieldPI);
