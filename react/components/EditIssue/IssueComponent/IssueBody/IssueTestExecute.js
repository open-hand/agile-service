import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Link } from 'react-router-dom';
import { testExecuteLink } from '@/utils/link';
import Divider from './Divider';

@observer class IssueTestExecute extends Component {
  renderExecutes = () => {
    const { store } = this.props;
    const { testExecutes } = store;
    return testExecutes.map((execute) => {
      const { executeId, summary, executionStatusName } = execute;
      return (
        <div style={{
          display: 'flex',
          alignItems: 'center',
          padding: '8px 10px',
          cursor: 'pointer',
          borderBottom: '1px solid rgba(0, 0, 0, 0.12)',
          borderTop: '1px solid rgba(0, 0, 0, 0.12)',
        }}
        >
          <Link to={testExecuteLink(executeId)}>
            {summary}
          </Link>
          <div style={{
            display: 'inline-block',
            fontSize: '.1rem',
            borderRadius: '.02rem',
            textAlign: 'center',
            padding: '0 .05rem',
            background: 'rgb(244, 67, 54)',
            height: '20px',
            lineHeight: '20px',
            marginRight: '15px',
            marginLeft: 'auto',
            color: '#fff',
          }}
          >
            {executionStatusName}
          </div>
        </div>
      );
    });
  }

  render() {
    return (
      <div id="link_executes">
        <Divider />
        <div className="c7n-title-wrapper">
          <div className="c7n-title-left">
            <span>测试执行</span>
          </div>

        </div>
        {this.renderExecutes()}
      </div>
    );
  }
}

export default IssueTestExecute;
