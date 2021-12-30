import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Icon, Button, Tooltip } from 'choerodon-ui/pro';
import { map, groupBy } from 'lodash';
import LinkList from '../../Component/LinkList';
import Divider from './Divider';
import openCreateLink from './create-link/CreateLink';

@observer class IssueLink extends Component {
  constructor(props) {
    super(props);
  }

  componentDidMount() {
  }

  onCreateLinkIssue = (res) => {
    const { reloadIssue, onLinkIssue } = this.props;
    if (reloadIssue) {
      reloadIssue();
    }
    onLinkIssue(res);
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
        showAssignee
        projectId={projectId}
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
    const group = typeCode !== 'feature' ? groupBy(linkIssues, 'ward') : groupBy(linkIssues, 'relationName');
    return (
      <div className="c7n-tasks">
        {
          map(group, (v, k) => (
            <div key={k}>
              <div style={{ margin: '7px auto' }}>{k}</div>
              {
                map(v, (linkIssue, i) => this.renderLinkList(linkIssue, i))
              }
            </div>
          ))
        }
      </div>
    );
  }

  render() {
    const { store, disabled, checkDescriptionEdit } = this.props;
    const issue = store.getIssue;
    const { issueId, typeCode } = issue;

    return (
      <div id="link_task">
        <Divider />
        <div className="c7n-title-wrapper">
          <div className="c7n-title-left">
            <span>{typeCode === 'feature' ? '关联Feature' : '关联工作项'}</span>
          </div>
          {!disabled && (
            <div className="c7n-title-right" style={{ marginLeft: '14px' }}>
              <Tooltip placement="topRight" title={typeCode === 'feature' ? '创建关联Feature' : '创建关联工作项'}>
                <Button
                  onClick={() => {
                    if (checkDescriptionEdit && !checkDescriptionEdit()) {
                      return;
                    }
                    openCreateLink({ issueId, projectId: store.projectId, onOk: this.onCreateLinkIssue });
                  }}
                >
                  <Icon type="playlist_add icon" />
                </Button>
              </Tooltip>
            </div>
          )}
        </div>
        {this.renderLinkIssues()}
      </div>
    );
  }
}

export default IssueLink;
