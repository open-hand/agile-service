/* eslint-disable react/require-default-props */
/* eslint-disable react/state-in-constructor */
import React, { Component, createRef } from 'react';
import PropTypes from 'prop-types';
import { Choerodon } from '@choerodon/boot';
import {
  Icon, Dropdown, Input, Menu,
} from 'choerodon-ui';
import { Button } from 'choerodon-ui/pro';
import { debounce, find } from 'lodash';
import { issueApi, fieldApi } from '@/api';
import { checkCanQuickCreate, getQuickCreateDefaultObj } from '@/utils/quickCreate';
import { fields2Map } from '@/utils/defaultValue';
import localCacheStore from '@/stores/common/LocalCacheStore';
import TypeTag from '../TypeTag';
import './QuickCreateIssue.less';
import UserDropdown from '../UserDropdown';

const propTypes = {
  defaultPriority: PropTypes.number,
  issueTypes: PropTypes.shape([]),
  onCreate: PropTypes.func,
  mountCreate: PropTypes.bool,
};

class QuickCreateIssue extends Component {
  /**
   * 得到一个可用的issueTypeId
   * @param {*} issueTypes
   * @param {*} issueTypeId
   * @returns {issueTypeId|undefined}
   */
  static getCacheAvailableIssueType(issueTypes, issueTypeId) {
    if (!issueTypes || !issueTypes.length) {
      return undefined;
    }
    let newCurrentType = issueTypeId ? find((issueTypes), { id: issueTypeId }) : undefined;
    if (!newCurrentType) {
      const localTypeId = localCacheStore.getItem('agile.issue.type.common.selected');
      newCurrentType = localTypeId ? find((issueTypes), { id: localTypeId }) : undefined;
      newCurrentType = newCurrentType || issueTypes[0];
    }
    return newCurrentType;
  }

  constructor(props) {
    super(props);
    this.currentTemplate = '';
    this.state = {
      create: props.mountCreate,
      loading: false,
      currentTypeId: QuickCreateIssue.getCacheAvailableIssueType(props.issueTypes)?.id,
      summary: '',
    };
    this.userDropDownRef = createRef();
  }

  static getDerivedStateFromProps(nextProps, prevState) {
    if (!prevState.currentTypeId && nextProps.issueTypes.length > 0) {
      return {
        currentTypeId: QuickCreateIssue.getCacheAvailableIssueType(nextProps.issueTypes)?.id,
      };
    }
    return null;
  }

  componentDidUpdate(prevProps, prevState) {
    if (typeof prevProps.onCreateChange === 'function' && this.state.create !== prevState.create) {
      prevProps.onCreateChange(this.state.create);
    }
  }

  loadInitValue = async (currentIssueTypeId) => {
    const { summary } = this.state;
    const defaultSummary = await fieldApi.project(this.props.projectId).getSummaryDefaultValue(currentIssueTypeId);
    if (summary === this.currentTemplate) {
      this.currentTemplate = defaultSummary;
      this.setState({
        summary: defaultSummary,
      });
    }
  };

  handleChangeType = ({ key }) => {
    this.setState({
      currentTypeId: key,
    });
    this.loadInitValue(key);
  };

  handleCreate = debounce(async () => {
    const { currentTypeId, loading } = this.state;
    if (loading) {
      return;
    }
    const {
      issueTypes, relateIssueId, sprintId, epicId, versionIssueRelVOList: propsVersionIssueRelVOList, chosenFeatureId, projectId, isInProgram, cantCreateEvent,
    } = this.props;

    const assigneeId = this.userDropDownRef?.current?.selectedUser?.id;

    const { summary } = this.state;
    if (summary && summary.trim()) {
      this.setState({
        loading: true,
      });
      const currentType = issueTypes.find((t) => t.id === currentTypeId);
      if (!await checkCanQuickCreate(currentType.id, assigneeId, projectId)) { //
        if (!cantCreateEvent) {
          Choerodon.prompt('该工作项类型含有必填选项，请使用弹框创建');
          this.setState({
            loading: false,
          });
        } else {
          Choerodon.prompt('请填写标注的必填字段');
          if (this.props.summaryChange) {
            this.props.summaryChange(summary);
          }
          if (this.props.typeIdChange) {
            this.props.typeIdChange(currentType.id);
          }
          if (this.props.setDefaultSprint) {
            this.props.setDefaultSprint(sprintId);
          }
          if (this.props.assigneeChange) {
            this.props.assigneeChange(assigneeId, this.userDropDownRef?.current?.selectedUser);
          }
          this.setState({
            loading: false,
            create: false,
          });
          cantCreateEvent();
          this.setState({ summary: this.currentTemplate || '' });
        }
        return;
      }
      const {
        defaultPriority, onCreate,
      } = this.props;
      if (summary.trim() !== '') {
        const param = {
          schemeCode: 'agile_issue',
          issueTypeId: currentType.id,
          pageCode: 'agile_issue_create',
        };
        const fields = await fieldApi.project(projectId).getFields(param, projectId);
        const fieldsMap = fields2Map(fields);

        const issue = getQuickCreateDefaultObj({
          epicName: currentTypeId === 'issue_epic' ? summary.trim() : undefined,
          featureId: currentType.typeCode === 'story' ? chosenFeatureId : 0,
          assigneeId,
          relateIssueId,
          versionIssueRelVOList: propsVersionIssueRelVOList,
          sprintId,
          summary,
          issueTypeId: currentType.id,
          typeCode: currentType.typeCode,
          priorityId: defaultPriority.id,
          epicId,
        }, fieldsMap);

        await issueApi.project(projectId).create(issue).then((res) => {
          this.setState({
            loading: false,
            create: false,
            summary: '',
          });
          const dto = {
            schemeCode: 'agile_issue',
            issueTypeId: currentType.id, // res.issueTypeId,
            pageCode: 'agile_issue_create',
          };
          fieldApi.project(projectId).quickCreateDefault(res.issueId, dto);
          if (onCreate) {
            onCreate(res);
          }
          this.setState({ summary: this.currentTemplate || '' });
          localCacheStore.setItem('agile.issue.type.common.selected', currentType.id);
        }).catch(() => {
          this.setState({
            loading: false,
          });
        });
      }
    }
  }, 500, {
    leading: true,
  });

  handleCancel = () => {
    this.setState({
      create: false,
    });
    if (this.props.typeIdChange) {
      this.props.typeIdChange(undefined);
    }
    if (this.props.summaryChange) {
      this.props.summaryChange(undefined);
    }
    if (this.props.setDefaultSprint) {
      this.props.setDefaultSprint(undefined);
    }
  };

  /**
   * 获取一个issueType 以做展示
   * @returns
   */
  getCurrentType() {
    const { issueTypes } = this.props;
    const { currentTypeId } = this.state;
    return QuickCreateIssue.getCacheAvailableIssueType(issueTypes, currentTypeId) || {};
  }

  render() {
    const {
      create, loading, currentTypeId, summary,
    } = this.state;
    const { issueTypes, buttonShowText, buttonShow = true } = this.props;

    const typeList = (
      <Menu
        style={{
          background: '#fff',
          boxShadow: '0 5px 5px -3px rgba(0, 0, 0, 0.20), 0 8px 10px 1px rgba(0, 0, 0, 0.14), 0 3px 14px 2px var(--divider)',
          borderRadius: '2px',
        }}
        onClick={this.handleChangeType}
      >
        {
          issueTypes.map((type) => (
            <Menu.Item key={type.id}>
              <div style={{ display: 'flex', justifyContent: 'space-between', alignItems: 'center' }}>
                <TypeTag
                  data={type}
                  showName
                />
              </div>
            </Menu.Item>
          ))
        }
      </Menu>
    );
    return (
      <div
        className="c7nagile-QuickCreateIssue"
        style={{
          userSelect: 'none',
          background: 'white',
          fontSize: 13,
          display: 'flex',
          alignItems: 'center',
        }}
      >
        {
          create ? (
            <div style={{ display: 'block', width: '100%' }}>
              <div style={{ display: 'flex', alignItems: 'center' }}>
                {
                  buttonShow && (
                    <Dropdown overlay={typeList} trigger={['click']}>
                      <div style={{ display: 'flex', alignItems: 'center', cursor: 'pointer' }}>
                        <TypeTag
                          data={this.getCurrentType()}
                        />
                        <Icon
                          type="arrow_drop_down"
                          style={{ fontSize: 16 }}
                        />
                      </div>
                    </Dropdown>
                  )
                }
                <UserDropdown projectId={this.props.projectId} userDropDownRef={this.userDropDownRef} defaultAssignee={this.props.defaultAssignee} key={this.props.defaultAssignee?.id || 'null'} />
                <Input
                  className="hidden-label"
                  autoFocus
                  autoComplete="on"
                  onPressEnter={this.handleCreate}
                  onChange={(e) => {
                    this.setState({
                      summary: e.target.value,
                    });
                  }}
                  value={summary}
                  maxLength={44}
                  placeholder="请输入工作项概要"
                />
                <Button
                  color="primary"
                  funcType="raised"
                  className="c7nagile-QuickCreateIssue-creating-btn"
                  disabled={this.props.issueTypes.length === 0 || !summary}
                  onClick={this.handleCreate}
                  style={{ marginLeft: 10 }}
                  loading={loading}
                >
                  确定
                </Button>
                <Button
                  className="c7nagile-QuickCreateIssue-creating-btn"
                  funcType="raised"
                  onClick={this.handleCancel}
                >
                  取消
                </Button>
              </div>
            </div>
          ) : (
            <Button
              funcType="flat"
              icon="playlist_add"
              disabled={this.props.issueTypes.length === 0}
              onClick={() => {
                this.setState({
                  create: true,
                }, () => {
                  this.loadInitValue(this.getCurrentType().id);
                });
              }}
              style={this.props.btnStyle || {}}
            >
              {buttonShowText || '创建工作项'}
            </Button>
          )
        }
      </div>
    );
  }
}

QuickCreateIssue.propTypes = propTypes;

export default QuickCreateIssue;
