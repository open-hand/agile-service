import React, { Component } from 'react';
import { Table, Tooltip } from 'choerodon-ui';
import { LINK_URL_TO } from '@/constants/LINK_URL';

class BurndownTable extends Component {
  renderDetail(item, record) {
    let result = '-';
    if (record.type !== 'startSprint' && record.type !== 'endSprint') {
      if (item.statistical) {
        if (item.oldValue !== item.newValue) {
          result = `由${item.oldValue}到${item.newValue}`;
        }
      }
    }
    return (
      <p style={{
        maxWidth: '100px',
        whiteSpace: 'nowrap',
        textOverflow: 'ellipsis',
        overflow: 'hidden',
      }}
      >
        {result}
      </p>
    );
  }

  renderUp(item) {
    let result = '-';
    if (item.newValue > item.oldValue) {
      if (item.statistical) {
        result = item.newValue - item.oldValue;
        if (result && result % 1 > 0) {
          result = result.toFixed(1);
        }
      }
    }
    return result;
  }

  renderDown(item) {
    let result = '-';
    if (item.newValue < item.oldValue) {
      if (item.statistical) {
        result = item.oldValue - item.newValue;
        if (result && result % 1 > 0) {
          result = result.toFixed(1);
        }
      }
    }
    return result;
  }

  judgeText(text) {
    let result = '';
    if (text === 'startSprint') {
      result = '开启冲刺';
    }
    if (text === 'addDuringSprint') {
      result = '在冲刺期间添加';
    }
    if (text === 'removeDoneDuringSprint') {
      result = '在冲刺期间从已完成到一个其他状态';
    }
    if (text === 'timespent') {
      result = '用户登记工作日志';
    }
    if (text === 'removeDuringSprint') {
      result = '冲刺中移除';
    }
    if (text === 'endSprint') {
      result = '关闭冲刺';
    }
    if (text === 'addDoneDuringSprint') {
      result = '在冲刺期间移动到已完成';
    }
    if (text === 'timeestimate') {
      result = '用户修改剩余时间';
    }
    if (text === 'valueChange') {
      if (this.props.select === 'remainingEstimatedTime') {
        result = '用户修改剩余预估时间';
      } else if (this.props.select === 'storyPoints') {
        result = '用户修改故事点';
      } else {
        result = '用户修改工作项计数';
      }
    }
    return result;
  }

  renderTypeText(text) {
    const splitArray = text.split('-');
    return (
      <div>
        {
          splitArray.map((item) => (
            <Tooltip mouseEnterDelay={0.5} title={`事件类型：${this.judgeText(item)}`}>
              <p style={{
                maxWidth: 150, overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap',
              }}
              >
                {this.judgeText(item)}

              </p>
            </Tooltip>
          ))
        }
      </div>
    );
  }

  render() {
    const { select, loading, data } = this.props;
    let unit = '';
    if (select === 'remainingEstimatedTime') {
      unit = '(小时)';
    }
    if (select === 'storyPoints') {
      unit = '(点)';
    }
    if (select === 'issueCount') {
      unit = '(个)';
    }
    const columns = [{
      // title: (
      //   <span>日期<Icon type={ dateSort === 'asc' ? 'arrow_upward' : 'arrow_downward' } style={{ cursor: 'pointer', marginTop: -4, marginLeft: 4 }} onClick={this.handleChangeSort} /></span>
      // ),
      title: '日期',
      dataIndex: 'date',
      key: 'date',
      width: '16%',
      render: (text) => (
        <Tooltip mouseEnterDelay={0.5} title={`日期：${text}`}>
          <p style={{
            maxWidth: '130px',
            whiteSpace: 'nowrap',
            textOverflow: 'ellipsis',
            overflow: 'hidden',
          }}
          >
            {text}
          </p>
        </Tooltip>
      ),
    }, {
      title: '工作项',
      dataIndex: 'issues',
      key: 'issues',
      width: '22%',
      render: (text, record) => (
        <div>
          {
            text.map((item) => (
              <p
                className="primary"
                style={{
                  maxWidth: '130px',
                  whiteSpace: 'nowrap',
                  textOverflow: 'ellipsis',
                  overflow: 'hidden',
                  cursor: 'pointer',
                }}
                role="none"
                onClick={() => {
                  if (item.parentIssueId) {
                    LINK_URL_TO.issueLinkTo(item.parentIssueId, item.issueNum, { paramOpenIssueId: item.issueId });
                  } else {
                    LINK_URL_TO.issueLinkTo(item.issueId, item.issueNum, { paramOpenIssueId: item.issueId });
                  }
                }}
              >
                <Tooltip mouseEnterDelay={0.5} title={`工作项：${item.parentIssueId ? `${item.parentIssueNum}/${item.issueNum}` : item.issueNum}`}>
                  {item.parentIssueId ? `${item.parentIssueNum}/${item.issueNum}` : item.issueNum}
                </Tooltip>
              </p>
            ))
          }
        </div>
      ),
    }, {
      title: '事件类型',
      dataIndex: 'type',
      key: 'type',
      width: '17%',
      render: (text) => (
        <div>{this.renderTypeText(text)}</div>
      ),
    }, {
      title: '事件详情',
      dataIndex: 'detail',
      key: 'detail',
      width: '13.5%',
      render: (text, record) => (
        <div className="textDisplayOverflow">
          {
            record.issues.map((item) => (
              <div>
                {this.renderDetail(item, record)}
              </div>
            ))
          }
        </div>
      ),
    }, {
      title: `升${unit}`,
      dataIndex: 'up',
      key: 'up',
      width: '10.5%',
      render: (text, record) => (
        <div>
          {
            record.issues.map((item) => (
              <div style={{ minWidth: 15 }}>
                {this.renderUp(item)}
              </div>
            ))
          }
        </div>
      ),
    }, {
      title: `降${unit}`,
      dataIndex: 'down',
      key: 'down',
      width: '10.5%',
      render: (text, record) => (
        <div className="textDisplayOverflow">
          {
            record.issues.map((item) => (
              <div style={{ minWidth: 15 }}>
                {this.renderDown(item)}
              </div>
            ))
          }
        </div>
      ),
    }, {
      title: `剩余${unit}`,
      dataIndex: 'rest',
      key: 'rest',
      width: '10.5%',
      render: (text) => (
        <p style={{ minWidth: 15 }}>{text}</p>
      ),
    }];

    return (
      <Table
        dataSource={data}
        loading={loading}
        columns={columns}
        pagination={false}
        rowKey={(record) => `${record.date}-${record.type}`}
      />
    );
  }
}

export default BurndownTable;
