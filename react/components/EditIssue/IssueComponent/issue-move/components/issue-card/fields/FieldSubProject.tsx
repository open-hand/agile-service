import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectTeam from '@/components/select/select-team';
import TeamTags from '@/components/tag/team-tags';
import { FieldCommonProps } from '../Field';

export interface FieldSubProjectProps extends FieldCommonProps {

}

const FieldSubProject: React.FC<FieldSubProjectProps> = ({ target, onChange, fieldWithValue }) => {
  const handleChange = useCallback((value) => {
    onChange && onChange(value?.map((v: any) => v.projectId), value);
  }, [onChange]);
  return (
    <TextEditToggle
      onSubmit={handleChange}
      initValue={fieldWithValue?.valueStr}
      mountRenderEditor={false}
      submitTrigger={['blur']}
      editor={() => (
        <SelectTeam
          multiple
          projectId={target.projectId}
          primitiveValue={false}
        />
      )}
    >
      <TeamTags
        data={fieldWithValue?.valueStr}
      />
    </TextEditToggle>
  );
};

export default observer(FieldSubProject);
