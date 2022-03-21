/* eslint-disable react/no-deprecated */
import React, { Component } from 'react';
import { observer, inject } from 'mobx-react';
import {
  Modal, Form, Input, DatePicker, Button,
} from 'choerodon-ui';
import moment from 'moment';
import { stores } from '@choerodon/boot';
import { versionApi, permissionApi } from '@/api';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import ReleaseStore from '../../../stores/project/release/ReleaseStore';

const { Sidebar } = Modal;
const { TextArea } = Input;
const FormItem = Form.Item;
const { AppState } = stores;

@observer
class EditRelease extends Component {
  constructor(props) {
    super(props);
    this.state = {
      startDate: null,
      expectReleaseDate: null,
      loading: false,
      editPermission: true,
    };
  }

  componentWillMount() {
    this.setState({
      startDate: ReleaseStore.getVersionDetail.startDate ? moment(ReleaseStore.getVersionDetail.startDate, 'YYYY-MM-DD HH:mm:ss') : null,
      expectReleaseDate: ReleaseStore.getVersionDetail.expectReleaseDate ? moment(ReleaseStore.getVersionDetail.expectReleaseDate, 'YYYY-MM-DD HH:mm:ss') : null,
    });
  }

  componentDidMount() {
    const codes = (AppState.currentMenuType.categories || []).map((c) => c.code);
    const permissions = codes.includes('N_WATERFALL')
      ? 'choerodon.code.project.cooperation.sprint.work-list.ps.updateversion'
      : 'choerodon.code.project.cooperation.work-list.ps.choerodon.code.cooperate.worklist.updateversion';
    permissionApi.check([permissions]).then((res) => {
      this.setState({
        editPermission: res.find((item) => item.code === permissions).approve,
      });
    });
  }

  handleOk = (e) => {
    e.preventDefault();
    const {
      form, onCancel, refresh, data,
    } = this.props;
    form.validateFields((err, value, modify) => {
      if (!err && modify) {
        this.setState({
          loading: true,
        });
        const newData = {
          description: value.description,
          name: value.name.trim(),
          objectVersionNumber: data.objectVersionNumber,
          projectId: AppState.currentMenuType.id,
          startDate: value.startDate ? `${moment(value.startDate).format('YYYY-MM-DD')} 00:00:00` : null,
          expectReleaseDate: value.expectReleaseDate ? `${moment(value.expectReleaseDate).format('YYYY-MM-DD')} 00:00:00` : null,
          // versionId: ReleaseStore.getVersionDetail.versionId,
        };
        versionApi.update(
          ReleaseStore.getVersionDetail.versionId, newData,
        ).then((res) => {
          this.setState({
            loading: false,
          });
          onCancel();
          refresh();
        }).catch((error) => {
          this.setState({
            loading: false,
          });
        });
      } else if (!modify) {
        onCancel();
      }
    });
  };

  checkName = (rule, value, callback) => {
    const proId = AppState.currentMenuType.id;
    const data = JSON.parse(JSON.stringify(ReleaseStore.getVersionDetail));
    if (value && value.trim() && data.name !== value.trim()) {
      versionApi.checkName(value.trim()).then((res) => {
        if (res) {
          callback('版本名称重复');
        } else {
          callback();
        }
      }).catch((error) => {
      });
    } else {
      callback();
    }
  };

  render() {
    const {
      loading, expectReleaseDate, startDate, editPermission,
    } = this.state;
    const { form, visible, onCancel } = this.props;
    const { getFieldDecorator } = form;
    const data = JSON.parse(JSON.stringify(ReleaseStore.getVersionDetail));
    this.state.startDate = ReleaseStore.getVersionDetail.startDate ? moment(ReleaseStore.getVersionDetail.startDate, 'YYYY-MM-DD HH:mm:ss') : null;
    return (
      <Sidebar
        maskClosable
        title="修改发布计划"
        visible={visible}
        destroyOnClose
        confirmLoading={loading}
        width={MODAL_WIDTH.small}
        footer={[
          <Button key="submit" type="primary" funcType="raised" loading={loading} onClick={this.handleOk} disabled={!editPermission}>
            确定
          </Button>,
          <Button key="back" onClick={onCancel} funcType="raised">取消</Button>,
        ]}
      >
        {
          visible ? (
            <Form>
              <FormItem>
                {getFieldDecorator('name', {
                  initialValue: data.name ? data.name : null,
                  rules: [{
                    required: true,
                    message: '版本名称必填',
                    whitespace: true,
                  }, {
                    validator: this.checkName,
                  }],
                })(
                  <Input label="版本名称" maxLength={15} />,
                )}
              </FormItem>
              <FormItem>
                {getFieldDecorator('startDate', {
                  initialValue: data.startDate ? moment(data.startDate, 'YYYY-MM-DD HH-mm-ss') : null,
                })(
                  <DatePicker
                    style={{ width: '100%' }}
                    label="开始日期"
                    placeholder="请选择开始日期"
                    disabledDate={expectReleaseDate
                      ? (current) => current > moment(expectReleaseDate) : () => false}
                    onChange={(date) => {
                      this.setState({
                        startDate: date,
                      });
                    }}
                  />,
                )}
              </FormItem>
              <FormItem>
                {getFieldDecorator('expectReleaseDate', {
                  initialValue: data.expectReleaseDate ? moment(data.expectReleaseDate, 'YYYY-MM-DD HH-mm-ss') : null,
                })(
                  <DatePicker
                    style={{ width: '100%' }}
                    label="预计发布日期"
                    placeholder="请选择预计发布日期"
                    disabledDate={startDate
                      ? (current) => current < moment(startDate) : () => false}
                    onChange={(date) => {
                      this.setState({
                        expectReleaseDate: date,
                      });
                    }}
                  />,
                )}
              </FormItem>
              <FormItem>
                {getFieldDecorator('description', {
                  initialValue: data.description ? data.description : null,
                })(
                  <TextArea label="版本描述" autosize={{ minRows: 3, maxRows: 10 }} maxLength={30} />,
                )}
              </FormItem>
            </Form>
          ) : ''
        }
      </Sidebar>
    );
  }
}

export default Form.create()(EditRelease);
