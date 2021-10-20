import React from 'react';
import { Select } from 'choerodon-ui/pro';
import useProjectIssueTypes from '@/hooks/data/useProjectIssueTypes';
import TypeTag from '@/components/TypeTag';

const { Option } = Select;
const SelectSubTask = (props) => {
  const { data: issueTypes } = useProjectIssueTypes({ typeCode: 'sub_task', onlyEnabled: true, projectId: props.projectId });
  return (
    <Select {...props} clearButton={false}>
      {issueTypes.map((type) => (
        <Option value={type.id}>
          <TypeTag
            data={type}
            showName
          />
        </Option>
      ))}
    </Select>
  );
};

export default SelectSubTask;
