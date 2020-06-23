import React, { Component } from 'react';
import _ from 'lodash';
import {
  Modal, Form, Input, Select, Icon,
} from 'choerodon-ui';
import {
  stores, Content, Choerodon,
} from '@choerodon/boot';
import { devOpsApi } from '@/api';
import './CreateBranch.less';
import './commom.less';

const { AppState } = stores;
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
      this.Select.focus();
    });
  }

  handleOk = (e) => {
    e.preventDefault();
    const { form, issueId, onOk } = this.props;
    form.validateFieldsAndScroll((err, values) => {
      if (!err) {
        const devopsBranchVO = {
          branchName: values.type === 'custom' ? values.name : `${values.type}-${values.name}`,
          issueId,
          originBranch: values.branch,
        };
        const applicationId = values.app;
        const projectId = AppState.currentMenuType.id;
        this.setState({
          confirmLoading: true,
        });
        devOpsApi.createBranch(applicationId, devopsBranchVO).then((res) => {
          this.setState({
            confirmLoading: false,
          });
          onOk();
        }).catch((error) => {
          this.setState({
            confirmLoading: false,
          });
        });
      }
    });
  };

  checkName = (rule, value, callback) => {
    // eslint-disable-next-line no-useless-escape
    const endWith = /(\/|\.|\.lock)$/;
    const contain = /(\s|~|\^|:|\?|\*|\[|\\|\.\.|@\{|\/{2,}){1}/;
    const single = /^@+$/;
    if (endWith.test(value)) {
      callback('不能以"/"、"."、".lock"结尾');
    } else if (contain.test(value) || single.test(value)) {
      callback('只能包含字母、数字、\'——\'、\'_\'');
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

  render() {
    const {
      visible, store, form, form: { getFieldDecorator },
      onCancel, issueNum, typeCode,
    } = this.props;
    const {
      confirmLoading, selectLoading, branchLoading,
      originApps, branchs, branchsObj, branchsSize,
      branchsInput, tags, tagsObj, tagsSize,
    } = this.state;
    return (
      <Sidebar
        className="c7nagile-createBranch"
        title="创建分支"
        visible={visible}
        onOk={this.handleOk}
        onCancel={onCancel}
        okText="创建"
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
              {getFieldDecorator('app', {
                rules: [{ required: true, message: '请选择应用' }],
              })(
                <Select
                  ref={(select) => { this.Select = select; }}
                  defaultOpen
                  label="应用名称"
                  allowClear
                  onFocus={this.onApplicationNameChange}
                  filter
                  optionFilterProp="children"
                  filterOption={
                    (input, option) => option.props.children.toLowerCase()
                      .indexOf(input.toLowerCase()) >= 0
                  }
                  loading={selectLoading}
                >
                  {originApps.map(app => (
                    <Option value={app.id} key={app.id}>{app.name}</Option>
                  ))}
                </Select>,
              )}
            </FormItem>
            <FormItem className="branch-formItem">
              {getFieldDecorator('branch', {
                rules: [{ required: true, message: '请选择分支来源' }],
              })(
                <Select
                  label="分支来源"
                  allowClear
                  disabled={!form.getFieldValue('app')}
                  filter
                  filterOption={false}
                  optionLabelProp="value"
                  loading={branchLoading}
                  onFilterChange={(input) => {
                    this.setState({
                      branchsInput: input,
                    });
                    devOpsApi.loadBranchesByService(form.getFieldValue('app'), undefined, undefined, {
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
                    devOpsApi.loadTagsByService(form.getFieldValue('app'), undefined, undefined, {
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
                    {branchs.map(s => (
                      <Option value={s.branchName} key={s.branchName}>
                        <Icon type="branch" className="c7nagile-name-icon" />
                        {s.branchName}
                      </Option>
                    ))}
                    {
                      branchsObj.nextPage ? (
                        <Option key="more">
                          <div
                            role="none"
                            style={{
                              margin: '-4px -20px',
                              padding: '4px 20px',
                              color: '#3f51b5',
                            }}
                            onClick={(e) => {
                              e.stopPropagation();
                              devOpsApi.loadBranchesByService(form.getFieldValue('app'), 1, branchsSize + 5, {
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
                    {tags.map(s => (
                      <Option value={s.name} key={s.name}>
                        <Icon type="local_offer" className="c7nagile-name-icon" />
                        {s.name}
                      </Option>
                    ))}
                    {
                      tagsObj.nextPage > 0 ? (
                        <Option key="more">
                          <div
                            role="none"
                            style={{
                              margin: '-4px -20px',
                              padding: '4px 20px',
                              color: '#3f51b5',
                            }}
                            onClick={(e) => {
                              e.stopPropagation();
                              devOpsApi.loadTagsByService(form.getFieldValue('app'), 1, tagsSize + 5, { 
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
                >
                  {['feature', 'bugfix', 'release', 'hotfix', 'custom'].map(s => (
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
