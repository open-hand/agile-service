/* eslint-disable no-param-reassign */
import React, { Component } from 'react';
import {
  Modal, Form, Select, Icon,
} from 'choerodon-ui';
import {
  Content, Choerodon,
} from '@choerodon/boot';
import { find } from 'lodash';
import { getProjectId } from '@/utils/common';
import { devOpsApi } from '@/api';
import SelectApp from '@/components/CreateBranch/SelectApp';
import './LinkBranch.less';
import './commom.less';


const { Sidebar } = Modal;
const { Option } = Select;
const FormItem = Form.Item;
class LinkBranch extends Component {
  constructor(props) {
    super(props);
    this.state = {
      // name: 'feature',
      // value: '',
      confirmLoading: false,
      selectLoading: true,
      branchLoading: true,
      originApps: [],
      branchs: [],
      branchsInput: '',
      page: 1,
      pageSize: 5,
      branchsObj: {},
    };
  }

  componentDidMount() {
    setTimeout(() => {
      if (this.Select) {
        this.Select.focus();
      }
    });
  }

  handleOk = (e) => {
    e.preventDefault();
    const { form, issueId, onOk } = this.props;
    form.validateFieldsAndScroll((err, values) => {
      if (err && (values.app || values.app2)) {
        delete err.app;
        delete err.app2;
      }
      if (!err || Object.keys(err).length === 0) {
        const applicationId = values.app || values.app2;
        const { branch } = values;
        const branchData = find(this.state.branchs, { branchName: branch });
        if (!branchData) {
          return;
        }
        const devopsBranchVO = {
          objectVersionNumber: branchData.objectVersionNumber,
          appServiceId: applicationId,
          branchName: branch,
          issueId,
        };
        this.setState({
          confirmLoading: true,
        });
        devOpsApi.project(this.getProjectId()).linkBranch(applicationId, devopsBranchVO).then(() => {
          this.setState({
            confirmLoading: false,
          });
          onOk();
        }).catch(() => {
          this.setState({
            confirmLoading: false,
          });
        });
      }
    });
  };

  getProjectId = () => {
    const { form } = this.props;
    const changeProject = form.getFieldValue('app2');
    return changeProject ? this.projectId : getProjectId();
  }

  onApplicationNameChange = () => {
    // this.setState({ selectLoading: true });
    devOpsApi.loadActiveService().then((res) => {
      this.setState({
        originApps: res,
        selectLoading: false,
        branchLoading: true,
      });
    });
  };

  handleSourceChange = () => {
    const { form } = this.props;
    form.resetFields(['app', 'app2', 'branch']);
  }

  handleAppChange = () => {
    const { form } = this.props;
    form.resetFields(['branch']);
    setTimeout(() => {
      form.validateFields(['name'], { force: true });
    });
  }

  handleOtherAppChange = (appId, projectId) => {
    this.projectId = projectId;
    this.handleAppChange();
  }

  getApp = () => {
    const { form } = this.props;
    return form.getFieldValue('app') || form.getFieldValue('app2');
  }

  render() {
    const {
      visible, form: { getFieldDecorator, getFieldValue }, issueId,
      onCancel,
    } = this.props;
    const {
      confirmLoading, selectLoading, branchLoading,
      originApps, branchs, branchsObj, pageSize,
      branchsInput, page,
    } = this.state;
    const source = getFieldValue('source') || 'self';
    const app = this.getApp();
    return (
      <Sidebar
        className="c7nagile-createBranch"
        title="添加关联分支"
        visible={visible}
        onOk={this.handleOk}
        onCancel={onCancel}
        okText="添加"
        cancelText="取消"
        confirmLoading={confirmLoading}
        width={740}
      >
        <Content
          style={{
            padding: 0,
            width: 512,
          }}
        >
          <Form layout="vertical" className="c7nagile-sidebar-form c7nagile-form">
            <FormItem className="branch-formItem">
              {getFieldDecorator('source', {
                rules: [{ required: true, message: '请选择服务来源' }],
                initialValue: 'self',
              })(
                <Select label="服务来源" onChange={this.handleSourceChange}>
                  <Option value="self">
                    本项目
                  </Option>
                  <Option value="other">
                    其他项目
                  </Option>
                </Select>,
              )}
            </FormItem>
            <FormItem className="branch-formItem">
              {source === 'self' ? getFieldDecorator('app', {
                rules: [{ required: true, message: '请选择应用' }],
              })(
                <Select
                  key="self"
                  ref={(select) => { this.Select = select; }}
                  defaultOpen
                  label="应用服务"
                  allowClear
                  onFocus={this.onApplicationNameChange}
                  filter
                  optionFilterProp="children"
                  filterOption={
                    (input, option) => option.props.children.toLowerCase()
                      .indexOf(input.toLowerCase()) >= 0
                  }
                  loading={selectLoading}
                  onChange={this.handleAppChange}
                >
                  {originApps.map(a => (
                    <Option value={a.id} key={a.id}>{a.name}</Option>
                  ))}
                </Select>,
              ) : getFieldDecorator('app2', {
                rules: [{ required: true, message: '请选择应用' }],
              })(<SelectApp key="other" onAppChange={this.handleOtherAppChange} />)}
            </FormItem>
            <FormItem className="branch-formItem">
              {getFieldDecorator('branch', {
                rules: [{ required: true, message: '请选择分支' }],
              })(
                <Select
                  label="分支"
                  allowClear
                  disabled={!app}
                  filter
                  filterOption={false}
                  optionLabelProp="value"
                  loading={branchLoading}
                  onFilterChange={(input) => {
                    this.setState({
                      branchsInput: input,
                    });
                    devOpsApi.project(this.getProjectId()).loadBranchesByServiceFilterIssue(app, undefined, pageSize, {
                      searchParam: {
                        branchName: input,
                      },
                      param: '',
                    }, issueId).then((res) => {
                      if (res && !res.failed) {
                        this.setState({
                          page: 1,
                          branchs: res.list,
                          branchsObj: res,
                          branchLoading: false,
                        });
                      } else {
                        Choerodon.prompt(res.message);
                      }
                    });
                  }}
                >
                  {branchs.map(s => (
                    <Option value={s.branchName} key={s.branchName}>
                      <Icon type="branch" className="c7nagile-name-icon" />
                      {s.branchName}
                    </Option>
                  ))}
                  {
                    branchsObj.hasNextPage ? (
                      <Option key="more-branch">
                        <div
                          role="none"
                          style={{
                            margin: '-4px -20px',
                            padding: '4px 20px',
                            color: '#5365EA',
                          }}
                          onClick={(e) => {
                            e.stopPropagation();
                            devOpsApi.loadBranchesByServiceFilterIssue(app, page + 1, pageSize, {
                              searchParam: {
                                branchName: branchsInput,
                              },
                              param: null,
                            }, issueId).then((res) => {
                              if (res && !res.failed) {
                                this.setState(state => ({
                                  page: page + 1,
                                  branchs: state.branchs.concat(res.list || []),
                                  branchsObj: res,
                                }));
                              } else {
                                Choerodon.prompt(res.message);
                              }
                            });
                          }}
                        >
                          查看更多
                        </div>
                      </Option>
                    ) : null
                  }
                </Select>,
              )}
            </FormItem>
          </Form>
        </Content>
      </Sidebar>
    );
  }
}
export default Form.create({})(LinkBranch);
