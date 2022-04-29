import React, { useCallback } from 'react';
import { observer } from 'mobx-react-lite';
import TextEditToggle from '@/components/TextEditTogglePro';
import SelectNumber from '@/components/select/select-number';
import { FieldCommonProps } from '../Field';

const FieldEstimateTime: React.FC<FieldCommonProps> = ({
  onChange, fieldWithValue,
}) => {
  const handleChange = useCallback((value) => {
    onChange && onChange(value, value);
  }, [onChange]);
  return (
    <TextEditToggle
      onSubmit={handleChange}
      mountRenderEditor={false}
      initValue={fieldWithValue?.valueStr}
      submitTrigger={['blur', 'change']}
      editor={() => (
        <SelectNumber style={{ height: 30 }} />
      )}
    >
      {fieldWithValue?.valueStr ? `${fieldWithValue?.valueStr} 小时` : '无'}
    </TextEditToggle>
  );
};

export default observer(FieldEstimateTime);
