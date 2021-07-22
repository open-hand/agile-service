import React from 'react';
import { Tooltip, Icon } from 'choerodon-ui/pro';
import { Row, Col } from 'choerodon-ui';
import SelectNumber from '@/components/select/select-number';

function WSJF() {
  return (
    <div>
      <h3 style={{ marginLeft: 10 }}>
        WSJF
        <Tooltip title="加权最短作业优先（WSJF）适用于对作业（例如功能，功能和史诗）进行排序以产生最大的经济效益的优先级模型。在SAFe中，WSJF估算为延迟成本（CoD）除以工作规模">
          <Icon type="help" style={{ color: 'var(--text-color3)', marginLeft: 2, marginTop: -5 }} />
        </Tooltip>
      </h3>
      <Row gutter={24}>
        <Col
          style={{ marginBottom: 24 }}
          span={12}
        >
          <SelectNumber style={{ width: '100%' }} name="userBusinessValue" label="用户/业务价值" />
        </Col>
        <Col
          style={{ marginBottom: 24 }}
          span={12}
        >
          <SelectNumber style={{ width: '100%' }} name="timeCriticality" label="时间紧迫性" />
        </Col>
        <Col
          style={{ marginBottom: 24 }}
          span={12}
        >
          <SelectNumber style={{ width: '100%' }} name="rrOeValue" label="降低风险|促成机会" />
        </Col>
        <Col
          style={{ marginBottom: 24 }}
          span={12}
        >
          <SelectNumber style={{ width: '100%' }} name="jobSize" label="用户/工作规模" />
        </Col>
      </Row>
    </div>
  );
}

export default WSJF;
