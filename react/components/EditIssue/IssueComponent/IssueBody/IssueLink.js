import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Icon, Button, Tooltip } from 'choerodon-ui/pro';
import _ from 'lodash';
import CreateLinkTask from '../../../CreateLinkTask';
import LinkList from '../../Component/LinkList';
import Divider from './Divider';

@observer class IssueLink extends Component {
  constructor(props) {
    super(props);
    this.state = {
      createLinkTaskShow: false,
    };
  }

  componentDidMount() {
  }

  handleCreateLinkIssue() {
    const { reloadIssue } = this.props;
    this.setState({
      createLinkTaskShow: false,
    });
    if (reloadIssue) {
      reloadIssue();
    }
  }

  renderLinkList(link, i) {
    const {
      reloadIssue, store, disabled, push, projectId, organizationId,
    } = this.props;
    const { issueId: id } = store.getIssue;
    return (
      <LinkList
        issue={{
          ...link,
          typeCode: link.typeCode,
        }}
        i={i}
        onOpen={(issueId, linkedIssueId) => {
          const rightId = issueId === id ? linkedIssueId : issueId;
          push({
            path: 'issue',
            props: {
              issueId: rightId,
              projectId,
              organizationId,
            },
          });
        }}
        onRefresh={() => {
          reloadIssue(id);
        }}
        canDelete={!disabled}
      />
    );
  }

  renderLinkIssues() {
    const { store } = this.props;
    const issue = store.getIssue;
    const { typeCode } = issue;
    const linkIssues = store.getLinkIssues;
    // const group = _.groupBy(linkIssues.filter(i => i.applyType === 'agile'), 'ward');
    const group = typeCode !== 'feature' ? _.groupBy(linkIssues, 'ward') : _.groupBy(linkIssues, 'relationName');
    return (
      <div className="c7n-tasks">
        {
          _.map(group, (v, k) => (
            <div key={k}>
              <div style={{ margin: '7px auto' }}>{k}</div>
              {
                  _.map(v, (linkIssue, i) => this.renderLinkList(linkIssue, i))
                }
            </div>
          ))
        }
      </div>
    );
  }

  render() {
    const { createLinkTaskShow } = this.state;
    const { store, disabled } = this.props;
    const issue = store.getIssue;
    const { issueId, typeCode } = issue;

    return (
      <div id="link_task">
        <Divider />
        <div className="c7n-title-wrapper">
          <div className="c7n-title-left">
            <span>{typeCode === 'feature' ? '关联Feature' : '关联问题'}</span>
          </div>
          {!disabled && (
          <div className="c7n-title-right" style={{ marginLeft: '14px' }}>
            <Tooltip placement="topRight" title={typeCode === 'feature' ? '创建关联Feature' : '创建关联问题'} getPopupContainer={(triggerNode) => triggerNode.parentNode}>
              <Button onClick={() => this.setState({ createLinkTaskShow: true })}>
                <Icon type="playlist_add icon" />
              </Button>
            </Tooltip>
          </div>
          )}
        </div>
        {this.renderLinkIssues()}
        {
          createLinkTaskShow ? (
            <CreateLinkTask
              issue={issue}
              issueId={issueId}
              issueType={typeCode}
              visible={createLinkTaskShow}
              onCancel={() => this.setState({ createLinkTaskShow: false })}
              onOk={this.handleCreateLinkIssue.bind(this)}
            />
          ) : null
        }
      </div>
    );
  }
}

export default IssueLink;
