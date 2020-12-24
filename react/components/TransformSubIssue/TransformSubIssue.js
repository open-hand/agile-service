import React, { Component } from 'react';
import { stores } from '@choerodon/boot';
import _ from 'lodash';
import {
  Modal, Form, Select, Tooltip,
} from 'choerodon-ui';
import { issueApi, statusApi } from '@/api';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import TypeTag from '../TypeTag';

import './TransformSubIssue.less';

const { AppState } = stores;
const { Sidebar } = Modal;
const FormItem = Form.Item;
const { Option } = Select;
const STATUS_COLOR = {
  todo: 'rgb(255, 177, 0)',
  doing: 'rgb(77, 144, 254)',
  done: 'rgb(0, 191, 165)',
};
let sign = false;

class TransformSubIssue extends Component {
  debounceFilterIssues = _.debounce((input) => {
    const { issueId } = this.props;
    this.setState({
      selectLoading: true,
    });
    issueApi.loadIssuesInLink(1, 20, issueId, input).then((res) => {
      this.setState({
        originIssues: res.list,
        selectLoading: false,
      });
    });
  }, 500);

  constructor(props) {
    super(props);
    this.state = {
      selectLoading: true,
      selectDefaultValue: undefined,
      originIssues: [],
      originStatus: [],
    };
  }

  componentDidMount() {
    this.getStatus();
    this.onFilterChange('');
    setTimeout(() => {
      this.Select.focus();
    });
  }

  onFilterChange=(input) => {
    const { issueId } = this.props;
    if (!sign) {
      this.setState({
        selectLoading: true,
      });
      issueApi.loadIssuesInLink(1, 20, issueId, input).then((res) => {
        this.setState({
          originIssues: res.list,
          selectLoading: false,
        });
      });
      sign = true;
    } else {
      this.debounceFilterIssues(input);
    }
  }

  getStatus() {
    this.setState({
      selectLoading: true,
    });
    const { issueTypes } = this.props;
    const issueTypeData = issueTypes;
    const subTask = issueTypeData.find((t) => t.typeCode === 'sub_task');
    if (subTask) {
      statusApi.loadAllForIssueType(subTask.id).then((res) => {
        this.setState({
          selectLoading: false,
          originStatus: res,
        });
      });
      statusApi.loadFirstInWorkFlow(subTask.id).then((res) => {
        this.setState({
          selectDefaultValue: String(res),
        });
      });
    } else {
      this.setState({
        selectLoading: false,
        originStatus: [],
      });
    }
  }

  handleTransformSubIssue = () => {
    const {
      form, onOk, issueTypes, issueId, ovn,
    } = this.props;
    form.validateFields((err, values) => {
      if (!err) {
        const issueTypeData = issueTypes;
        const subTask = issueTypeData.find((t) => t.typeCode === 'sub_task');
        const issueTransformSubTask = {
          issueId,
          parentIssueId: values.issuesId,
          statusId: values.statusId,
          objectVersionNumber: ovn,
          issueTypeId: subTask && subTask.id,
          typeCode: subTask && subTask.typeCode,
        };
        this.setState({
          loading: true,
        });
        issueApi.taskTransformSubTask(issueTransformSubTask)
          .then((res) => {
            this.setState({
              loading: false,
            });
            onOk();
          });
      }
    });
  };

  render() {
    const {
      form,
      visible,
      onCancel,
      issueNum,
    } = this.props;
    const {
      loading,
      selectLoading,
      originIssues,
      originStatus,
      selectDefaultValue,
    } = this.state;
    const { getFieldDecorator } = form;

    return (
      <Sidebar
        maskClosable={false}
        className="c7n-transformSubIssue"
        title="转化为子问题"
        visible={visible || false}
        onOk={this.handleTransformSubIssue}
        onCancel={onCancel}
        okText="转化"
        cancelText="取消"
        confirmLoading={loading}
        width={MODAL_WIDTH.small}
      >
        <Form layout="vertical">
          <FormItem label="父任务">
            {getFieldDecorator('issuesId', {
              rules: [{ required: true, message: '请选择父任务' }],
            })(
              <Select
                ref={(select) => { this.Select = select; }}
                defaultOpen
                label="父任务"
                loading={selectLoading}
                filter
                filterOption={false}
                onFilterChange={this.onFilterChange}
              >
                {originIssues.map((issue) => (
                  <Option
                    key={issue.issueId}
                    value={issue.issueId}
                  >
                    <Tooltip title={issue.summary}>
                      <div style={{ display: 'inline-flex', width: 'calc(100% - 30px)', flex: 1 }}>
                        <div>
                          <TypeTag
                            data={issue.issueTypeVO}
                          />
                        </div>
                        <a style={{
                          paddingLeft: 12,
                          paddingRight: 12,
                          overflow: 'hidden',
                          textOverflow: 'ellipsis',
                          whiteSpace: 'nowrap',
                        }}
                        >
                          {issue.issueNum}
                        </a>
                        <div style={{ overflow: 'hidden', flex: 1 }}>
                          <p style={{
                            paddingRight: '25px',
                            overflow: 'hidden',
                            textOverflow: 'ellipsis',
                            whiteSpace: 'nowrap',
                            marginBottom: 0,
                            maxWidth: 'unset',
                          }}
                          >
                            {issue.summary}
                          </p>
                        </div>
                      </div>
                    </Tooltip>

                  </Option>
                ))}
              </Select>,
            )}
          </FormItem>
          <FormItem label="状态">
            {getFieldDecorator('statusId', {
              rules: [{ required: true, message: '请选择状态' }],
              initialValue: selectDefaultValue,
            })(
              <Select
                label="状态"
                loading={selectLoading}
              >
                {
                  originStatus.map((status) => (
                    <Option key={status.id} value={status.id}>
                      <div style={{ display: 'inline-flex', alignItems: 'center' }}>
                        <div
                          style={{
                            width: 15,
                            height: 15,
                            background: STATUS_COLOR[status.type],
                            marginRight: 6,
                            borderRadius: '2px',
                          }}
                        />
                        {status.name}
                      </div>
                    </Option>
                  ))
                }
              </Select>,
            )}
          </FormItem>
        </Form>
      </Sidebar>
    );
  }
}
export default Form.create({})(TransformSubIssue);
