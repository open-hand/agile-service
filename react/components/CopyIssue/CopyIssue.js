import React, { Component } from 'react';
import { stores } from '@choerodon/boot';
import _ from 'lodash';
import {
  Modal, Form, Input, Checkbox,
} from 'choerodon-ui';

import './CopyIssue.less';
import { epicApi, issueApi } from '@/api';

const { AppState } = stores;
const FormItem = Form.Item;

class CopyIssue extends Component {
  constructor(props) {
    super(props);
    this.state = {
      loading: false,
    };
  }

  componentDidMount() {
    setTimeout(() => {
      this.textInput.focus();
    });
  }

  handleCopyIssue = () => {
    const { applyType = 'agile' } = this.props;
    this.props.form.validateFields((err, values) => {
      if (!err) {
        const projectId = AppState.currentMenuType.id;
        const orgId = AppState.currentMenuType.organizationId;
        const {
          issueId, issue,
        } = this.props;
        const {
          issueSummary, issueName, copySubIssue, copyLinkIssue, sprint,
        } = values;
        const copyConditionVO = {
          issueLink: copyLinkIssue || false,
          sprintValues: sprint || false,
          subTask: copySubIssue || false,
          summary: issueSummary || false,
          epicName: issueName || false,
        };
        this.setState({
          loading: true,
        });
        issueApi.clone(issueId, applyType, copyConditionVO)
          .then((res) => {
            this.setState({
              loading: false,
            });
            this.props.onOk(res);
          });
      }
    });
  };

  checkEpicNameRepeat = (rule, value, callback) => {
    if (value && value.trim()) {
      epicApi.checkName(value)
        .then((res) => {
          if (res) {
            callback('史诗名称重复');
          } else {
            callback();
          }
        });
    } else {
      callback();
    }
  };

  render() {
    const {
      visible, onCancel, issueNum, issueSummary, issue,
    } = this.props;
    const { getFieldDecorator } = this.props.form;

    return (
      <Modal
        className="c7n-copyIssue"
        title={`复制问题${issueNum}`}
        visible={visible || false}
        onOk={this.handleCopyIssue}
        onCancel={onCancel}
        okText="复制"
        cancelText="取消"
        confirmLoading={this.state.loading}
        maskClosable={false}
        keyboard={false}
      >
        <Form layout="vertical" style={{ width: 472 }}>
          <FormItem style={{ marginTop: 20 }}>
            {getFieldDecorator('issueSummary', {
              rules: [{ required: true, message: '请输入概要' }],
              initialValue: issueSummary,
            })(
              <Input
                ref={(input) => { this.textInput = input; }}
                label="概要"
                maxLength={44}
              />,
            )}
          </FormItem>
          {
            issue.typeCode === 'issue_epic' && (
            <FormItem style={{ marginTop: 20 }}>
              {getFieldDecorator('issueName', {
                rules: [{ required: true, message: '请输入史诗名称' },
                  { validator: this.checkEpicNameRepeat }],
                initialValue: issue.epicName,
              })(
                <Input
                  ref={(input) => { this.textInput = input; }}
                  label="名称"
                  maxLength={20}
                />,
              )}
            </FormItem>
            )
          }
          {
            this.props.issue.closeSprint.length || this.props.issue.activeSprint ? (
              <FormItem>
                {getFieldDecorator('sprint', {})(
                  <Checkbox>
                    是否复制冲刺
                  </Checkbox>,
                )}
              </FormItem>
            ) : null
          }
          {
            this.props.issue.subIssueVOList.length ? (
              <FormItem>
                {getFieldDecorator('copySubIssue', {})(
                  <Checkbox>
                    是否复制子任务
                  </Checkbox>,
                )}
              </FormItem>
            ) : null
          }
          {
            this.props.issueLink.length ? (
              <FormItem>
                {getFieldDecorator('copyLinkIssue', {})(
                  <Checkbox>
                    是否复制关联任务
                  </Checkbox>,
                )}
              </FormItem>
            ) : null
          }
        </Form>
      </Modal>
    );
  }
}
export default Form.create({})(CopyIssue);
