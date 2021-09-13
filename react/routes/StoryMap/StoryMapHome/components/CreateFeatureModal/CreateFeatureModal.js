import React, { Component } from 'react';
import { observer } from 'mobx-react';
import {
  Modal, Form, Input, Select,
} from 'choerodon-ui';
import { Content, stores } from '@choerodon/boot';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { issueApi, fieldApi } from '@/api';
import StoryMapStore from '../../../../../stores/project/StoryMap/StoryMapStore';

const { AppState } = stores;
const { Sidebar } = Modal;
const FormItem = Form.Item;
const { TextArea } = Input;
const { Option } = Select;

class CreateFeatureModal extends Component {
  constructor(props) {
    super(props);
    this.state = {
      loading: false,
    };
  }

  componentDidUpdate(prevProps) {
    // eslint-disable-next-line react/destructuring-assignment
    if (this.props.visible && !prevProps.visible) {
      setTimeout(() => {
        this.input.focus();
      });
    }
  }

  /**
   *
   * 创建特性
   * @param {*} e
   * @memberof CreateFeature
   */
  handleCreateFeature =(e) => {
    const {
      form, onCancel, featureType, defaultPriority, onOk,
    } = this.props;
    const defaultPriorityId = defaultPriority ? defaultPriority.id : '';
    e.preventDefault();
    form.validateFieldsAndScroll((err, value) => {
      if (!err) {
        const req = {
          projectId: AppState.currentMenuType.id,
          summary: value.summary,
          typeCode: 'feature',
          issueTypeId: featureType && featureType.id,
          priorityCode: `priority-${defaultPriorityId}`,
          priorityId: defaultPriorityId,
          featureVO: {
            // benfitHypothesis: values.benfitHypothesis,
            // acceptanceCritera: values.acceptanceCritera,
            featureType: value.featureType,
          },
        };
        this.setState({
          loading: true,
        });
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
          onOk(res);
        }).catch((error) => {
          this.setState({
            loading: false,
          });
        });
      }
    });
  };

  render() {
    const {
      form, onCancel, visible,
    } = this.props;
    const { loading } = this.state;
    const { getFieldDecorator } = form;
    return (
      <Sidebar
        maskClosable
        title="创建特性"
        visible={visible}
        okText="创建"
        cancelText="取消"
        onCancel={() => {
          form.resetFields();
          onCancel();
        }}
        confirmLoading={loading}
        onOk={this.handleCreateFeature}
        width={MODAL_WIDTH.middle}
      >
        <Content
          style={{
            padding: 0,
          }}
        >
          <Form style={{ width: 512 }}>
            <FormItem>
              {getFieldDecorator('summary', {
                rules: [{
                  required: true,
                  message: '特性名称不能为空',
                  // transform: value => value && value.trim(),
                },
                // {
                //   validator: this.checkEpicNameRepeat,
                // }
                ],
              })(
                <Input ref={(input) => { this.input = input; }} label="特性名称" maxLength={44} />,
              )}
            </FormItem>
            <FormItem style={{ width: 520 }}>
              {getFieldDecorator('featureType', {
                rules: [{ required: true, message: '特性类型为必输项' }],
                initialValue: 'business',
              })(
                <Select
                  label="特性类型"
                  
                >
                  <Option key="business" value="business">
                    特性
                  </Option>
                  <Option key="enabler" value="enabler">
                    使能
                  </Option>
                </Select>,
              )}
            </FormItem>
          </Form>
        </Content>
      </Sidebar>
    );
  }
}

const CreateFeatureModalContainer = observer(({ ...props }) => {
  const { createFeatureModalVisible } = StoryMapStore;
  return (
    <CreateFeatureModal
      visible={createFeatureModalVisible}
      featureType={StoryMapStore.getFeatureType}
      defaultPriority={StoryMapStore.getDefaultPriority}
      onCancel={() => {
        StoryMapStore.setCreateFeatureModalVisible(false);
      }}
      {...props}
    />
  );
});
export default Form.create()(CreateFeatureModalContainer);
