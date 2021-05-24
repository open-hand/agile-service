import React, { Component } from 'react';
import { observer } from 'mobx-react';
import {
  Modal, Form, Input,
} from 'choerodon-ui';
import { stores, Choerodon } from '@choerodon/boot';
import {
  epicApi, issueApi, fieldApi,
} from '@/api';
import { checkCanQuickCreate } from '@/utils/quickCreate';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import BacklogStore from '../../../../stores/project/backlog/BacklogStore';

const { AppState } = stores;
const { Sidebar } = Modal;
const FormItem = Form.Item;
const { TextArea } = Input;

@Form.create({})
@observer
class CreateEpic extends Component {
  constructor(props) {
    super(props);
    this.state = {
      loading: false,
    };
  }

  /**
   *
   * 创建史诗
   * @param {*} e
   * @memberof CreateEpic
   */
  handleCreateEpic =(e) => {
    const {
      form, onCancel, refresh,
    } = this.props;
    const issueTypes = BacklogStore.getIssueTypes || [];
    const defaultPriorityId = BacklogStore.getDefaultPriority ? BacklogStore.getDefaultPriority.id : '';
    e.preventDefault();
    form.validateFieldsAndScroll(async (err, value) => {
      if (!err) {
        const epicType = issueTypes.find((t) => t.typeCode === 'issue_epic');
        const req = {
          projectId: AppState.currentMenuType.id,
          epicName: value.name.trim(),
          summary: value.summary.trim(),
          typeCode: 'issue_epic',
          issueTypeId: epicType && epicType.id,
          priorityCode: `priority-${defaultPriorityId}`,
          priorityId: defaultPriorityId,
        };
        this.setState({
          loading: true,
        });
        if (!await checkCanQuickCreate(epicType.id)) {
          if (!this.props.cantCreateEvent) {
            Choerodon.prompt('该问题类型含有必填选项，请使用创建问题弹框创建');
            this.setState({
              loading: false,
            });
          } else {
            Choerodon.prompt('请填写标注的必填字段');
            if (this.props.summaryChange) {
              this.props.summaryChange(value.summary.trim());
            }
            if (this.props.typeIdChange) {
              this.props.typeIdChange(epicType && epicType.id);
            }
            if (this.props.epicNameChange) {
              this.props.epicNameChange(value.name.trim());
            }
            this.setState({
              loading: false,
            });
            onCancel();
            this.props.cantCreateEvent();
            form.resetFields();
          }
          return;
        }
        issueApi.create(req).then((res) => {
          const dto = {
            schemeCode: 'agile_issue',
            issueTypeId: res.issueTypeId,
            pageCode: 'agile_issue_create',
          };
          fieldApi.quickCreateDefault(res.issueId, dto);
          this.setState({
            loading: false,
          });
          form.resetFields();
          refresh();
          onCancel();
        }).catch((error) => {
          this.setState({
            loading: false,
          });
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
      form, onCancel, visible, store,
    } = this.props;
    const issueTypes = BacklogStore.getIssueTypes || [];
    const epicType = issueTypes.find((t) => t.typeCode === 'issue_epic');
    const { loading } = this.state;
    const { getFieldDecorator } = form;
    return (
      <Sidebar
        maskClosable={false}
        title="创建史诗"
        visible={visible}
        okText="创建"
        cancelText="取消"
        onCancel={() => {
          form.resetFields();
          onCancel();
        }}
        confirmLoading={loading}
        onOk={this.handleCreateEpic}
        width={MODAL_WIDTH.small}
      >
        <Form>
          <FormItem>
            {getFieldDecorator('name', {
              rules: [{
                required: true,
                message: '史诗名称不能为空',
                whitespace: true,
              }, {
                validator: this.checkEpicNameRepeat,
              }],
            })(
              <Input label="史诗名称" maxLength={20} />,
            )}
          </FormItem>
          <FormItem>
            {getFieldDecorator('summary', {
              rules: [{
                required: true,
                message: '概要不能为空',
                whitespace: true,
              }],
            })(
              <TextArea autosize={{ minRows: 3, maxRows: 10 }} label="概要" maxLength={44} />,
            )}
          </FormItem>
        </Form>
      </Sidebar>
    );
  }
}

export default CreateEpic;
