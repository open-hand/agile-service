import React, { Component } from 'react';
import { observer } from 'mobx-react';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import EmptyBacklog from './emptyBacklog.svg';
import emptySprint from './emptySprint.svg';
import './NoneIssue.less';

function SprintNoIssue() {
  return (
    <div className="c7n-noissue-wapper">
      <div style={{ display: 'flex', height: 100 }} className="c7n-noissue-notzero">
        <img style={{ width: 80, height: 70 }} alt="空sprint" src={emptySprint} />
        <div style={{ marginLeft: 20 }}>
          <p>计划您的SPRINT</p>
          <p>这是一个Sprint。将工作项拖拽至此来计划一个Sprint。</p>
        </div>
      </div>
    </div>
  );
}
function BacklogNoIssue() {
  return (
    <div
      style={{
        display: 'flex',
        justifyContent: 'center',
        alignItems: 'center',
        padding: '42px 0 45px 0',
      }}
    >
      <img style={{ width: 172 }} alt="emptybacklog" src={EmptyBacklog} />
      <div style={{ marginLeft: 40 }}>
        <p style={{ color: 'var(--text-color3)', fontSize: '13px' }}>当前项目暂无待办事项</p>
        <p style={{ fontSize: 16, lineHeight: '28px', marginTop: 8 }}>
          您可以在此创建并评估工作项，可通过上下拖动来规划工作项的排列顺序
        </p>
      </div>
    </div>
  );
}
@observer class NoneIssue extends Component {
  render() {
    const { type } = this.props;
    // eslint-disable-next-line no-nested-ternary
    return BacklogStore.hasFilter ? (
      <div className="c7n-noissue-wapper">
        <div className="c7n-noissue-notzero">{`在${type === 'backlog' ? 'backlog' : 'sprint'}中所有工作项已筛选`}</div>
      </div>
    ) : (
      type === 'backlog' ? <BacklogNoIssue /> : <SprintNoIssue />
    );
  }
}

export default NoneIssue;
