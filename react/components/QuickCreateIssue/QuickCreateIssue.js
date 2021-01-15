/* eslint-disable react/require-default-props */
/* eslint-disable react/state-in-constructor */
import React, { Component } from 'react';
import PropTypes from 'prop-types';
import { Choerodon } from '@choerodon/boot';
import {
  Button, Icon, Dropdown, Input, Menu, Form,
} from 'choerodon-ui';
import { debounce } from 'lodash';
import { getProjectId } from '@/utils/common';
import { issueApi, fieldApi } from '@/api';
import { checkCanQuickCreate } from '@/utils/quickCreate';
import { fields2Map } from '@/utils/defaultValue';
import TypeTag from '../TypeTag';
import './QuickCreateIssue.less';

const FormItem = Form.Item;

const propTypes = {
  defaultPriority: PropTypes.number,
  issueTypes: PropTypes.shape([]),
  onCreate: PropTypes.func,
};
@Form.create({})
class QuickCreateIssue extends Component {
  state = {
    create: false,
    loading: false,
    currentTypeCode: 'task',
  }

  handleChangeType = ({ key }) => {
    this.setState({
      currentTypeCode: key,
    });
  };

  handleCreate = debounce(() => {
    const { currentTypeCode } = this.state;
    const {
      form, issueTypes, sprintId, epicId, versionIssueRelVOList, chosenFeatureId,
    } = this.props;
    form.validateFields(async (err, values) => {
      const { summary } = values;
      if (summary && summary.trim()) {
        if (!err) {
          this.setState({
            loading: true,
          });
          const currentType = issueTypes.find((t) => t.typeCode === currentTypeCode);
          if (!await checkCanQuickCreate(currentType.typeCode)) {
            Choerodon.prompt('该问题类型含有必填选项，请使用弹框创建');
            this.setState({
              loading: false,
            });
            return;
          }
          const {
            defaultPriority, onCreate, defaultAssignee,
          } = this.props;
          if (summary.trim() !== '') {
            const param = {
              schemeCode: 'agile_issue',
              context: currentType.typeCode,
              pageCode: 'agile_issue_create',
            };
            const fields = await fieldApi.getFields(param);
            const fieldsMap = fields2Map(fields);
            const issue = {
              priorityCode: `priority-${defaultPriority.id}`,
              priorityId: defaultPriority.id,
              projectId: getProjectId(),
              programId: getProjectId(),
              epicId: epicId || 0,
              summary: summary.trim(),
              issueTypeId: currentType.id,
              typeCode: currentType.typeCode,
              parentIssueId: 0,
              relateIssueId: 0,
              featureVO: {},
              sprintId: sprintId || fieldsMap.get('sprint').defaultValue || 0,
              epicName: currentTypeCode === 'issue_epic' ? summary.trim() : undefined,
              componentIssueRelVOList: fieldsMap.get('component').defaultValueObjs || [],
              description: '',
              issueLinkCreateVOList: [],
              labelIssueRelVOList: fieldsMap.get('label').defaultValueObjs || [],
              versionIssueRelVOList: versionIssueRelVOList || [],
              fixVersionIssueRel: fieldsMap.get('fixVersion').defaultValue || [],
              featureId: currentType.typeCode === 'story' ? chosenFeatureId : 0,
              assigneeId: defaultAssignee,
            };
            issueApi.create(issue).then((res) => {
              this.setState({
                loading: false,
                create: false,
              });
              const dto = {
                schemeCode: 'agile_issue',
                context: res.typeCode,
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
      }
    });
  }, 500, {
    leading: true,
  });

  render() {
    const {
      create, loading, currentTypeCode,
    } = this.state;
    const { issueTypes, form: { getFieldDecorator } } = this.props;
    const currentType = issueTypes.find((t) => t.typeCode === currentTypeCode);

    const typeList = (
      <Menu
        style={{
          background: '#fff',
          boxShadow: '0 5px 5px -3px rgba(0, 0, 0, 0.20), 0 8px 10px 1px rgba(0, 0, 0, 0.14), 0 3px 14px 2px rgba(0, 0, 0, 0.12)',
          borderRadius: '2px',
        }}
        onClick={this.handleChangeType}
      >
        {
          issueTypes.map((type) => (
            <Menu.Item key={type.typeCode}>
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
            <Form style={{ width: '100%' }}>
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
                  <FormItem label="summary" style={{ flex: 1, margin: '0 10px', padding: 0 }}>
                    {getFieldDecorator('summary', {
                      rules: [{ required: true, message: '请输入问题概要！' }],
                    })(
                      <Input
                        className="hidden-label"
                        autoFocus
                        autoComplete="on"
                        onPressEnter={this.handleCreate}
                        maxLength={44}
                        placeholder="请输入问题概要"
                      />,
                    )}
                  </FormItem>
                  <Button
                    funcType="raised"
                    type="primary"
                    // htmlType="submit"
                    onClick={this.handleCreate}
                    style={{ margin: '0 10px' }}
                    loading={loading}
                  >
                    确定
                  </Button>
                  <Button
                    funcType="raised"
                    onClick={() => {
                      this.setState({
                        create: false,
                      });
                    }}
                  >
                    取消
                  </Button>
                </div>
              </div>
            </Form>
          ) : (
            <Button
              type="primary"
              icon="playlist_add"
              onClick={() => {
                this.setState({
                  create: true,
                });
              }}
            >
              创建问题
            </Button>
          )
        }
      </div>
    );
  }
}

QuickCreateIssue.propTypes = propTypes;

export default QuickCreateIssue;
