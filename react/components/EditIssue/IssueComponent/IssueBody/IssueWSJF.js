import React from 'react';
import { observer } from 'mobx-react-lite';
import { Tooltip, Icon } from 'choerodon-ui';
import { Button } from 'choerodon-ui/pro';
import Divider from './Divider';
import { FieldWSJFDTO } from './Field';
import './IssueWSJF.less';

const WSJFFIELDS = [
  {
    fieldCode: 'userBusinessValue',
    fieldName: '用户/业务价值',
  },
  {
    fieldCode: 'timeCriticality',
    fieldName: '时间紧迫性',
  },
  {
    fieldCode: 'rrOeValue',
    fieldName: '降低风险|促成机会',
  },
  {
    fieldCode: 'jobSize',
    fieldName: '工作规模',
  },
  {
    fieldCode: 'costDelay',
    fieldName: '延迟成本',
    disabled: true,
  },
  {
    fieldCode: 'wsjf',
    fieldName: 'WSJF',
    disabled: true,
  },
];

function IssueWSJF(props) {
  const { store } = props;
  const issue = store.getIssue;
  const wsjfDTOShow = store.getWSJFDTOShow;
  const { wsjf } = issue || {};
  const { wsjf: wsjfValue } = wsjf || {};
  return (
    <div id="wsjf">
      <Divider />
      <div className="c7n-title-wrapper">
        <div className="c7n-title-left">        
          <span>{`WSJF：${(wsjfValue || wsjfValue === 0) ? wsjfValue : '无'}`}</span>
          <Tooltip title="加权最短作业优先（WSJF）适用于对作业（例如功能，功能和史诗）进行排序以产生最大的经济效益的优先级模型。在SAFe中，WSJF估算为延迟成本（CoD）除以工作规模">
            <Icon type="help" style={{ color: 'var(--text-color3)', marginLeft: 2 }} />
          </Tooltip>
        </div>        
      </div>
      {
        wsjfDTOShow && (
          <div className="c7n-content-wrapper c7n-wsjf">
            {
              WSJFFIELDS.map(field => (<FieldWSJFDTO {...props} field={field} />))
            }
          </div>
        )
      }
      <Button className="leftBtn" onClick={() => store.setWSJFDTOShow(!wsjfDTOShow)}>
        <span>{wsjfDTOShow ? '收起' : '展开'}</span>
        <Icon type={wsjfDTOShow ? 'baseline-arrow_drop_up' : 'baseline-arrow_right'} style={{ marginRight: 2 }} />
      </Button>
    </div>
  );
}

export default observer(IssueWSJF);
