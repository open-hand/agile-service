import React, { Component } from 'react';
import PropTypes from 'prop-types';
import {
  Button, Icon, Dropdown, Input, Menu, Form,
} from 'choerodon-ui';
import TypeTag from '../TypeTag';
import { deBounce } from './Utils';
import { getProjectId } from '../../common/utils';
import { createIssue, createIssueField } from '../../api/NewIssueApi';
import './QuickCreateIssue.less';

const debounceCallback = deBounce(500);
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

  handleCreate = () => {
    const { currentTypeCode } = this.state;
    const {
      form, issueTypes, sprintId, epicId, versionIssueRelVOList,
    } = this.props;
    form.validateFields((err, values) => {
      const { summary } = values;
      if (summary && summary.trim()) {
        if (!err) {
          const currentType = issueTypes.find(t => t.typeCode === currentTypeCode);
          const {
            defaultPriority, onCreate,
          } = this.props;
          debounceCallback(() => {
            if (summary.trim() !== '') {
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
                sprintId: sprintId || 0,      
                epicName: currentTypeCode === 'issue_epic' ? summary.trim() : undefined,    
                componentIssueRelVOList: [],
                description: '',
                issueLinkCreateVOList: [],
                labelIssueRelVOList: [],
                versionIssueRelVOList: versionIssueRelVOList || [],
              };
              this.setState({
                loading: true,
              });
              createIssue(issue).then((res) => {
                this.setState({
                  loading: false,
                  create: false,
                });
                const dto = {
                  schemeCode: 'agile_issue',
                  context: res.typeCode,
                  pageCode: 'agile_issue_create',
                };
                
                createIssueField(res.issueId, dto);
                if (onCreate) {
                  onCreate(res);
                }
              }).catch(() => {
                this.setState({
                  loading: false,
                });
              });
            }
          }, this);
        }
      }
    });
  };

  render() {
    const {
      create, loading, currentTypeCode,
    } = this.state;
    const { issueTypes, form: { getFieldDecorator } } = this.props;    
    const currentType = issueTypes.find(t => t.typeCode === currentTypeCode);
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
          issueTypes.map(type => (
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
