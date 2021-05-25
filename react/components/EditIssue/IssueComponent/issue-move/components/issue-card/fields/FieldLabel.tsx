import React, { useCallback } from 'react';
import { Select } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import TextEditToggle from '@/components/TextEditTogglePro';
import LabelTags from '@/components/tag/label-tags';
import { FieldCommonProps } from '../Field';

export interface FieldLabelProps extends FieldCommonProps {

}

const { Option } = Select;

const FieldLabel: React.FC<FieldLabelProps> = ({ target, onChange, fieldWithValue }) => {
  const { labelList } = target;
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
        <Select multiple>
          {labelList.map((label) => (
            <Option value={label.labelId}>
              {label.labelName}
            </Option>
          ))}
        </Select>
      )}
    >
      <LabelTags
        data={fieldWithValue?.valueStr}
      />
    </TextEditToggle>
  );
};

export default observer(FieldLabel);
