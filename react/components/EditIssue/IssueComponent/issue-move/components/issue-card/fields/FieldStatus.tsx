import React, { useCallback } from 'react';
import { Select } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { find } from 'lodash';
import StatusTag from '@/components/StatusTag';
import TextEditToggle from '@/components/TextEditTogglePro';
import { FieldCommonProps } from '../Field';

export interface FieldStatusProps extends FieldCommonProps {

}

const { Option } = Select;

const FieldStatus: React.FC<FieldStatusProps> = ({
  target, field, onChange, fieldWithValue,
}) => {
  const { statusList } = target;
  const handleChange = useCallback((value) => {
    onChange && onChange(value, find(statusList, { id: value }));
  }, [onChange, statusList]);
  return (
    <TextEditToggle
      onSubmit={handleChange}
      initValue={fieldWithValue?.value}
      alwaysRender={false}
      submitTrigger={['blur', 'change']}
      editor={() => (
        <Select clearButton={false}>
          {statusList.map((status) => (
            <Option value={status.id}>
              {status.name}
            </Option>
          ))}
        </Select>
      )}
    >
      {fieldWithValue?.valueStr ? (
        <StatusTag
          data={fieldWithValue?.valueStr}
        />
      ) : '无'}
    </TextEditToggle>
  );
};

export default observer(FieldStatus);
