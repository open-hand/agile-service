import React from 'react';
import {
  Tooltip, Icon, Form, DataSet,
} from 'choerodon-ui/pro';
import SelectNumber from '@/components/select/select-number';

interface Props {
  dataSet: DataSet,
}

function WSJF({ dataSet }: Props) {
  return (
    <div>
      <h3 style={{ marginLeft: 10 }}>
        WSJF
        <Tooltip title="加权最短作业优先（WSJF）适用于对作业（例如功能，功能和史诗）进行排序以产生最大的经济效益的优先级模型。在SAFe中，WSJF估算为延迟成本（CoD）除以工作规模">
          <Icon type="help" style={{ color: 'var(--text-color3)', marginLeft: 2, marginTop: -5 }} />
        </Tooltip>
      </h3>
      <Form dataSet={dataSet} columns={2}>
        <SelectNumber name="userBusinessValue" label="用户/业务价值" />
        <SelectNumber name="timeCriticality" label="时间紧迫性" />
        <SelectNumber name="rrOeValue" label="降低风险|促成机会" />
        <SelectNumber name="jobSize" label="用户/工作规模" />
      </Form>
    </div>
  );
}

export default WSJF;
