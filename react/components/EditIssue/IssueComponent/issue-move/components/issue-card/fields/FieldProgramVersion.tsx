import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectProgramVersion from '@/components/select/select-program-version';
import ProgramVersionTags from '@/components/tag/program-version-tags';
import { FieldCommonProps } from '../Field';

export interface FieldProgramVersionProps extends FieldCommonProps {

}

const FieldProgramVersion: React.FC<FieldProgramVersionProps> = ({ target, onChange, fieldWithValue }) => {
  const handleChange = useCallback((value) => {
    onChange && onChange(value.issueId, value);
  }, [onChange]);
  return (
    <TextEditToggle
      onSubmit={handleChange}
      initValue={fieldWithValue?.valueStr}
      mountRenderEditor={false}
      submitTrigger={['blur']}
      editor={() => (
        <SelectProgramVersion
          multiple
          projectId={target.projectId}
          primitiveValue={false}
        />
      )}
    >
      <ProgramVersionTags
        data={fieldWithValue?.valueStr}
      />
    </TextEditToggle>
  );
};

export default observer(FieldProgramVersion);
