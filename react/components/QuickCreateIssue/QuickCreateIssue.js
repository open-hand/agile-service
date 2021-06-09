/* eslint-disable react/require-default-props */
/* eslint-disable react/state-in-constructor */
import React, { Component, createRef } from 'react';
import PropTypes from 'prop-types';
import { Choerodon } from '@choerodon/boot';
import {
  Icon, Dropdown, Input, Menu,
} from 'choerodon-ui';
import { Button } from 'choerodon-ui/pro';
import { debounce } from 'lodash';
import { issueApi, fieldApi } from '@/api';
import { checkCanQuickCreate, getQuickCreateDefaultObj } from '@/utils/quickCreate';
import { fields2Map } from '@/utils/defaultValue';
import TypeTag from '../TypeTag';
import './QuickCreateIssue.less';
import UserDropdown from '../UserDropdown';

const propTypes = {
  defaultPriority: PropTypes.number,
  issueTypes: PropTypes.shape([]),
  onCreate: PropTypes.func,
};

class QuickCreateIssue extends Component {
  constructor(props) {
    super(props);
    this.currentTemplate = undefined;
    this.state = {
      create: false,
      loading: false,
      currentTypeId: props.issueTypes[0]?.id,
      summary: '',
    };
    this.userDropDownRef = createRef();
  }

  static getDerivedStateFromProps(nextProps, prevState) {
    if (!prevState.currentTypeId && nextProps.issueTypes.length > 0) {
      return {
        currentTypeId: nextProps.issueTypes[0]?.id,
      };
    }
    return null;
  }

  loadInitValue = async (currentIssueTypeId) => {
    const { summary } = this.state;
    const defaultSummary = await fieldApi.getSummaryDefaultValue(currentIssueTypeId);
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
    const { currentTypeId } = this.state;
    const {
      issueTypes, relateIssueId, sprintId, epicId, versionIssueRelVOList: propsVersionIssueRelVOList, chosenFeatureId, isInProgram, cantCreateEvent,
    } = this.props;

    const assigneeId = this.userDropDownRef?.current?.selectedUser?.id;

    const { summary } = this.state;
    if (summary && summary.trim()) {
      this.setState({
        loading: true,
      });
      const currentType = issueTypes.find((t) => t.id === currentTypeId);
      if (!await checkCanQuickCreate(currentType.id, assigneeId)) { //
        if (!cantCreateEvent) {
          Choerodon.prompt('该问题类型含有必填选项，请使用弹框创建');
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
            this.props.assigneeChange(assigneeId);
          }
          this.setState({
            loading: false,
            create: false,
          });
          cantCreateEvent();
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
        const fields = await fieldApi.getFields(param);
        const fieldsMap = fields2Map(fields);

        const issue = getQuickCreateDefaultObj({
          epicName: currentTypeId === 'issue_epic' ? summary.trim() : undefined,
          featureId: currentType.typeCode === 'story' ? chosenFeatureId : 0,
          assigneeId,
          epicId,
          relateIssueId,
          versionIssueRelVOList: propsVersionIssueRelVOList,
          sprintId,
          summary,
          issueTypeId: currentType.id,
          typeCode: currentType.typeCode,
          priorityId: defaultPriority.id,
          epicId,
        }, fieldsMap);

        issueApi.create(issue).then((res) => {
          this.setState({
            loading: false,
            create: false,
          });
          const dto = {
            schemeCode: 'agile_issue',
            issueTypeId: currentType.id, // res.issueTypeId,
            pageCode: 'agile_issue_create',
          };
          fieldApi.quickCreateDefault(res.issueId, dto);
          if (onCreate) {
            onCreate(res);
          }
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

  render() {
    const {
      create, loading, currentTypeId, summary,
    } = this.state;
    const { issueTypes, buttonShowText } = this.props;
    const currentType = issueTypes.find((t) => t.id === currentTypeId);

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
                <Dropdown overlay={typeList} trigger={['click']}>
                  <div style={{ display: 'flex', alignItems: 'center' }}>
                    <TypeTag
                      data={currentType}
                    />
                    <Icon
                      type="arrow_drop_down"
                      style={{ fontSize: 16 }}
                    />
                  </div>
                </Dropdown>
                <UserDropdown userDropDownRef={this.userDropDownRef} defaultAssignee={this.props.defaultAssignee} key={this.props.defaultAssignee?.id || 'null'} />
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
                  placeholder="请输入问题概要"
                />
                <Button
                  color="primary"
                  funcType="raised"
                  disabled={this.props.issueTypes.length === 0 || !summary}
                  onClick={this.handleCreate}
                  style={{ margin: '0 10px' }}
                  loading={loading}
                >
                  确定
                </Button>
                <Button
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
                  this.loadInitValue(currentType.id);
                });
              }}
            >
              {buttonShowText || '创建问题'}
            </Button>
          )
        }
      </div>
    );
  }
}

QuickCreateIssue.propTypes = propTypes;

export default QuickCreateIssue;
