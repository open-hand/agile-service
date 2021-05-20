/* eslint-disable no-param-reassign */
import React, { Component } from 'react';
import {
  Modal, Form, Input, Select, Icon, Tooltip,
} from 'choerodon-ui';
import {
  Content, Choerodon,
} from '@choerodon/boot';
import { getProjectId } from '@/utils/common';
import { devOpsApi } from '@/api';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import SelectApp from './SelectApp';
import './CreateBranch.less';
import './commom.less';

const { Sidebar } = Modal;
const { Option, OptGroup } = Select;
const FormItem = Form.Item;
const MAP = {
  bug: 'bugfix',
  task: 'feature',
  story: 'feature',
  issue_epic: 'feature',
  sub_task: 'feature',
};

class CreateBranch extends Component {
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
      // branchsShowMore: false,
      branchsInput: '',
      branchsSize: 5,
      branchsObj: {},
      tags: [],
      // tagsShowMore: false,
      tagsSize: 5,
      tagsObj: {},
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
        const devopsBranchVO = {
          branchName: values.type === 'custom' ? values.name : `${values.type}-${values.name}`,
          issueId,
          originBranch: values.branch,
        };
        const applicationId = values.app || values.app2;
        this.setState({
          confirmLoading: true,
        });
        devOpsApi.project(this.getProjectId()).createBranch(applicationId, devopsBranchVO).then(() => {
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

  checkName = async (rule, value, callback) => {
    const { form } = this.props;
    const type = form.getFieldValue('type');
    const branchName = type === 'custom' ? value : `${type}-${value}`;
    // eslint-disable-next-line no-useless-escape
    const endWith = /(\/|\.|\.lock)$/;
    const contain = /(\s|~|\^|:|\?|\*|\[|\\|\.\.|@\{|\/{2,}){1}/;
    const single = /^@+$/;
    if (endWith.test(value)) {
      callback('不能以"/"、"."、".lock"结尾');
    } else if (contain.test(value) || single.test(value)) {
      callback('只能包含字母、数字、\'——\'、\'_\'');
    } else if (this.getApp() && !await devOpsApi.project(this.getProjectId()).checkBranchName(this.getApp(), branchName)) {
      callback('分支名称已存在');
    } else {
      callback();
    }
  };

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
      visible, store, form, form: { getFieldDecorator, getFieldValue },
      onCancel, issueNum, typeCode,
    } = this.props;
    const {
      confirmLoading, selectLoading, branchLoading,
      originApps, branchs, branchsObj, branchsSize,
      branchsInput, tags, tagsObj, tagsSize,
    } = this.state;
    const source = getFieldValue('source') || 'self';
    const app = this.getApp();
    return (
      <Sidebar
        maskClosable
        className="c7nagile-createBranch"
        title="创建分支"
        visible={visible}
        onOk={this.handleOk}
        onCancel={onCancel}
        okText="创建"
        cancelText="取消"
        confirmLoading={confirmLoading}
        width={MODAL_WIDTH.middle}
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
                    (input, option) => option.props.title.toLowerCase()
                      .indexOf(input.toLowerCase()) >= 0
                  }
                  loading={selectLoading}
                  onChange={this.handleAppChange}
                >
                  {originApps.map((a) => (
                    <Option value={a.id} key={a.id} title={`${a.name}(${a.code})`}>
                      <Tooltip title={a.code}>
                        {`${a.name}(${a.code})`}
                      </Tooltip>
                    </Option>
                  ))}
                </Select>,
              ) : getFieldDecorator('app2', {
                rules: [{ required: true, message: '请选择应用' }],
              })(<SelectApp key="other" onAppChange={this.handleOtherAppChange} />)}
            </FormItem>
            <FormItem className="branch-formItem">
              {getFieldDecorator('branch', {
                rules: [{ required: true, message: '请选择分支来源' }],
              })(
                <Select
                  label="分支来源"
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
                    devOpsApi.project(this.getProjectId()).loadBranchesByService(app, undefined, undefined, {
                      searchParam: {
                        branchName: input,
                      },
                      param: '',
                    }).then((res) => {
                      if (res && !res.failed) {
                        this.setState({
                          branchs: res.list,
                          branchsSize: res.total,
                          // branchsShowMore: res.totalPages !== 1,
                          branchsObj: res,
                          branchLoading: false,
                        });
                      } else {
                        Choerodon.prompt(res.message);
                      }
                    });
                    devOpsApi.project(this.getProjectId()).loadTagsByService(app, undefined, undefined, {
                      searchParam: {
                        tagName: input,
                      },
                      param: '',
                    }).then((res) => {
                      if (res && !res.failed) {
                        this.setState({
                          tags: res.list || [],
                          tagsSize: res.pageSize,
                          // tagsShowMore: res.totalPages !== 1,
                          tagsObj: res,
                        });
                      } else {
                        Choerodon.prompt(res.message);
                      }
                    });
                  }}
                >
                  <OptGroup label="分支" key="branchGroup">
                    {branchs.map((s) => (
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
                              devOpsApi.loadBranchesByService(app, 1, branchsSize + 5, {
                                searchParam: {
                                  branchName: branchsInput,
                                },
                                param: null,
                              }).then((res) => {
                                if (res && !res.failed) {
                                  this.setState({
                                    branchs: res.list || [],
                                    branchsSize: res.pageSize,
                                    branchsObj: res,
                                  });
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
                  </OptGroup>
                  <OptGroup label="tag" key="tagGroup">
                    {tags.map((s) => (
                      <Option value={s.name} key={s.name}>
                        <Icon type="local_offer" className="c7nagile-name-icon" />
                        {s.name}
                      </Option>
                    ))}
                    {
                      tagsObj.number < tagsObj.totalPages ? (
                        <Option key="more-tag">
                          <div
                            role="none"
                            style={{
                              margin: '-4px -20px',
                              padding: '4px 20px',
                              color: '#5365EA',
                            }}
                            onClick={(e) => {
                              e.stopPropagation();
                              devOpsApi.loadTagsByService(app, 1, tagsSize + 5, {
                                searchParam: {
                                  tagName: branchsInput,
                                },
                                param: null,
                              }).then((res) => {
                                if (res && !res.failed) {
                                  this.setState({
                                    tags: res.list || [],
                                    tagsSize: res.pageSize,
                                    // tagsShowMore: res.totalPages !== 1,
                                    tagsObj: res,
                                  });
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
                  </OptGroup>
                </Select>,
              )}
            </FormItem>
            <FormItem className="c7nagile-formItem_180">
              {getFieldDecorator('type', {
                rules: [{ required: true, message: '请选择分支类型' }],
                initialValue: MAP[typeCode || 'task'],
              })(
                <Select
                  allowClear
                  label="分支类型"
                  onChange={() => {
                    setTimeout(() => {
                      form.validateFields(['name'], { force: true });
                    });
                  }}
                >
                  {['feature', 'bugfix', 'release', 'hotfix', 'custom'].map((s) => (
                    <Option value={s} key={s}>
                      <span className={`c7nagile-branch-icon icon-${s === 'bugfix' ? 'develop' : s}`}>
                        {s.slice(0, 1).toUpperCase()}
                      </span>
                      <span>{s}</span>
                    </Option>
                  ))}
                </Select>,
              )}
            </FormItem>
            <FormItem className="c7nagile-formItem_281">
              {getFieldDecorator('name', {
                rules: [{
                  required: true,
                  message: '请输入分支名称',
                }, {
                  validator: this.checkName,
                }],
                initialValue: issueNum,
              })(
                <Input
                  label="分支名称"
                  // prefix={form.getFieldValue('type') === 'custom' || !form.getFieldValue('type') ? '' : `${form.getFieldValue('type')}-`}
                  maxLength={30}
                />,
              )}
            </FormItem>
          </Form>
        </Content>
      </Sidebar>
    );
  }
}
export default Form.create({})(CreateBranch);
