import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectFixVersion from '@/components/select/select-version';
import VersionTags from '@/components/tag/version-tags';
import { FieldCommonProps } from '../Field';

export interface FieldVersionProps extends FieldCommonProps {

}

const FieldVersion: React.FC<FieldVersionProps> = ({ target, onChange, fieldWithValue }) => {
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
        <SelectFixVersion
          multiple
          projectId={target.projectId}
          statusArr={['version_planning']}
          valueField="versionId"
          primitiveValue={false}
        />
      )}
    >
      <VersionTags
        data={fieldWithValue?.valueStr}
      />
    </TextEditToggle>
  );
};

export default observer(FieldVersion);
