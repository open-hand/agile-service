import React, { Component } from 'react';
import { Modal, Form, Input } from 'choerodon-ui';
import { Choerodon } from '@choerodon/boot';
import { observer } from 'mobx-react';
import { getProjectId } from '@/utils/common';
import { versionApi } from '@/api';
import StoryMapStore from '../../../../../stores/project/StoryMap/StoryMapStore';

const FormItem = Form.Item;
class CreateVersion extends Component {
  state = {
    loading: false,
  }

  handleCreate = () => {
    const { form, onOk } = this.props;
    const { resetFields } = form;
    form.validateFields((err, values) => {
      if (!err) {
        const projectId = getProjectId();
        const { name } = values;
        this.setState({
          loading: true,
        });
        // if (type === 'sprint') {
        //   // 创建冲刺
        //   axios.post(`/agile/v1/projects/${projectId}/sprint/create?sprintName=${name}`)
        //     .then((res) => {
        //       this.setState({ loading: false });
        //       onOk();
        //     })
        //     .catch((error) => {
        //       this.setState({ loading: false });
        //     });
        // } else {
        // 创建版本
        const versionCreateVO = {
          name,
          projectId,
          releaseDate: null,
          startDate: null,
        };
        versionApi.create(versionCreateVO).then((res) => {
          if (!res.failed) {
            resetFields();
            onOk(res);
          } else {
            Choerodon.promt(res.message);
          }
          this.setState({ loading: false });
        })
          .catch((error) => {
            this.setState({ loading: false });
          });
        // }
      }
    });
  };

  /**
   *验证版本名称是否重复
   *
   * @memberof CreateVersion
   */
  checkVersionNameRepeat = (rule, value, callback) => {
    versionApi.checkName(value.trim()).then((res) => {
      if (res) {
        callback('版本名称重复');
      } else {
        callback();
      }
    });
  };

  handleCancel = () => {
    const { onCancel, form } = this.props;
    const { resetFields } = form;
    resetFields();
    if (onCancel) {
      onCancel();
    }
  }

  render() {
    const { visible, form } = this.props;
    const { getFieldDecorator } = form;
    const { loading, nextSprintName } = this.state;
    const type = 'version';
    return (
      <Modal
        title={`创建${type === 'sprint' ? '冲刺' : '版本'}`}
        visible={visible}
        onOk={this.handleCreate}
        onCancel={this.handleCancel}
        okText="创建"
        cancelText="取消"
        destroyOnClose
        confirmLoading={loading}
      >
        <Form layout="vertical">
          <FormItem>
            {getFieldDecorator('name', {
              rules: [{ required: true, message: '请输入名称' },
                type === 'version' ? {
                  validator: this.checkVersionNameRepeat,
                } : {
                  validator: this.checkSprintNameRepeat,
                }],
              initialValue: nextSprintName,
            })(
              <Input
                label={`${type === 'sprint' ? '冲刺' : '版本'}名称`}
                onPressEnter={this.handleCreate}
                maxLength={type === 'sprint' ? 30 : 15}
              />,
            )}
          </FormItem>
        </Form>
      </Modal>
    );
  }
}

CreateVersion.propTypes = {

};
const CreateVersionContainer = observer(({ ...props }) => {
  const { createModalVisible } = StoryMapStore;
  return (
    <CreateVersion
      onCancel={() => {
        StoryMapStore.setCreateModalVisible(false);
      }}
      visible={createModalVisible}
      {...props}
    />
  );
});
export default Form.create({})(CreateVersionContainer);
