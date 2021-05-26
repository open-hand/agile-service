import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectSprint from '@/components/select/select-sprint';
import SprintTag from '@/components/tag/sprint-tag';
import { FieldCommonProps } from '../Field';

export interface FieldSprintProps extends FieldCommonProps {

}

const FieldSprint: React.FC<FieldSprintProps> = ({
  target, onChange, fieldWithValue, disabledSprint,
}) => {
  const handleChange = useCallback((value) => {
    onChange && onChange(value?.sprintId, value);
  }, [onChange]);

  return (
    <TextEditToggle
      disabled={disabledSprint}
      onSubmit={handleChange}
      initValue={fieldWithValue?.valueStr}
      mountRenderEditor={false}
      submitTrigger={['blur', 'change']}
      editor={() => (
        <SelectSprint projectId={target.projectId} primitiveValue={false} />
      )}
    >
      <SprintTag
        data={fieldWithValue?.valueStr}
      />
    </TextEditToggle>
  );
};

export default observer(FieldSprint);
