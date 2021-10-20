import React from 'react';
import { DataSet, Select } from 'choerodon-ui/pro';
import useStatus from '@/hooks/data/useStatus';

const { Option } = Select;

const STATUS_COLOR = {
  todo: 'rgb(255, 177, 0)',
  doing: 'rgb(77, 144, 254)',
  done: 'rgb(0, 191, 165)',
};

interface Props {
  issueTypeId?: string
  projectId?:string
  name: string
}

const SelectSubTaskStatus: React.FC<Props> = ({ issueTypeId, projectId, name }) => {
  const { data: statusList = [] } = useStatus({ issueTypeId, projectId }, {
    enabled: !!issueTypeId,

  });
  return (
    <Select name={name} clearButton={false} style={{ width: '100%' }}>
      {statusList.map((status) => (
        <Option key={status.id} value={status.id}>
          <div style={{ display: 'inline-flex', alignItems: 'center' }}>
            <div
              style={{
                width: 15,
                height: 15,
                // @ts-ignore
                background: STATUS_COLOR[status.type],
                marginRight: 6,
                borderRadius: '2px',
              }}
            />
            {status.name}
          </div>
        </Option>
      ))}
    </Select>
  );
};

export default SelectSubTaskStatus;
