import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectEpic from '@/components/select/select-epic';
import EpicTag from '@/components/tag/epic-tag';
import { FieldCommonProps } from '../Field';

export interface FieldEpicProps extends FieldCommonProps {

}

const FieldEpic: React.FC<FieldEpicProps> = ({ target, onChange, fieldWithValue }) => {
  const handleChange = useCallback((value) => {
    onChange && onChange(value?.issueId, value);
  }, [onChange]);
  return (
    <TextEditToggle
      onSubmit={handleChange}
      initValue={fieldWithValue?.valueStr}
      mountRenderEditor={false}
      submitTrigger={['blur', 'change']}
      editor={() => (
        <SelectEpic
          dontAddEpic0
          selectIds={fieldWithValue?.value}
          primitiveValue={false}
          projectId={target.projectId}
        />
      )}
    >
      <EpicTag
        data={fieldWithValue?.valueStr}
      />
    </TextEditToggle>
  );
};

export default observer(FieldEpic);
