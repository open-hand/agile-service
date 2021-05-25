import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectFeature from '@/components/select/select-feature';
import FeatureTag from '@/components/tag/feature-tag';
import { FieldCommonProps } from '../Field';

export interface FieldFeatureProps extends FieldCommonProps {

}

const FieldFeature: React.FC<FieldFeatureProps> = ({ target, onChange, fieldWithValue }) => {
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
        <SelectFeature
          primitiveValue={false}
          projectId={target.projectId}
        />
      )}
    >
      <FeatureTag
        data={fieldWithValue?.valueStr}
      />
    </TextEditToggle>
  );
};

export default observer(FieldFeature);
