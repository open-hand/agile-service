import React, { Component } from 'react';
import { observer } from 'mobx-react';
import {
  Icon, Modal, Radio, Select,
} from 'choerodon-ui';
import _ from 'lodash';
import { versionApi } from '@/api';

const RadioGroup = Radio.Group;
const { Option } = Select;

@observer
class DeleteReleaseWithIssue extends Component {
  constructor(props) {
    super(props);
    this.state = {
      distributed: true,
      targetVersionId: '',
    };
  }

  componentWillReceiveProps(nextProps) {
    if (nextProps.versionDelInfo.versionNames) {
      if (nextProps.versionDelInfo.versionNames.length > 0) {
        this.setState({
          targetVersionId: nextProps.versionDelInfo.versionNames[0].versionId,
          distributed: true,
        });
      } else {
        this.setState({
          targetVersionId: '',
          distributed: false,
        });
      }
    }
  }

  handleOk() {
    const {
      versionDelInfo, onCancel, refresh,
    } = this.props;
    const {
      distributed, targetVersionId,
    } = this.state;
    versionApi.delete(versionDelInfo.versionId,
      (versionDelInfo.agileIssueCount && distributed) ? targetVersionId : undefined).then((data) => {
      onCancel();
      refresh();
    }).catch((error) => {
    });
  }

  render() {
    const {
      visible, versionDelInfo, onCancel,
    } = this.props;
    return (
      <Modal
        title="删除版本"
        visible={visible}
        okText="删除"
        cancelText="取消"
        onCancel={onCancel.bind(this)}
        onOk={this.handleOk.bind(this)}
      >
        <p style={{ marginTop: 20, marginBottom: 0 }}>
          {`您正在删除 ${Object.keys(versionDelInfo).length ? versionDelInfo.versionName : ''} 版本`}
        </p>
        <div style={{ marginTop: 10 }}>
          {
            versionDelInfo.agileIssueCount > 0 || versionDelInfo.testCaseCount > 0 ? (
              <div style={{ marginBottom: 0 }}>
                <p style={{ flex: 1, marginBottom: 10 }}>
                  <Icon
                    type="error"
                    style={{
                      display: 'inline-block', marginRight: 6, marginTop: -3, color: 'red',
                    }}
                  />
                  {'此版本有'}
                  {
                    versionDelInfo.agileIssueCount ? (
                      <span>
                        <span style={{ color: 'red' }}>{` ${versionDelInfo.agileIssueCount} `}</span>
                        个工作项
                      </span>
                    ) : ''
                  }
                  {
                    versionDelInfo.testCaseCount ? (
                      <span>
                        ,
                        <span style={{ color: 'red' }}>{` ${versionDelInfo.testCaseCount} `}</span>
                        个测试用例
                      </span>
                    ) : ''
                  }
                  {'。相关的工作项将移动到下面选择的版本中。'}
                </p>
              </div>
            ) : ''
          }
          {
            versionDelInfo.testCaseCount ? (
              <div>
                <p>
                  注意：删除后与版本相关的测试用例会一并删除。
                </p>
              </div>
            ) : ''
          }
          {
            Object.keys(versionDelInfo).length && versionDelInfo.versionNames.length && versionDelInfo.agileIssueCount ? (
              <div style={{ marginTop: 20 }}>
                <div style={{ flex: 4 }}>
                  <Select
                    style={{
                      width: '100%',
                    }}
                    label="请选择要移动到版本"
                    onChange={(value) => {
                      this.setState({
                        targetVersionId: value,
                      });
                      if (value === -1) {
                        this.setState({
                          distributed: false,
                        });
                      } else {
                        this.setState({
                          distributed: true,
                        });
                      }
                    }}
                    defaultValue={versionDelInfo.versionNames[0].versionId}
                  >
                    {
                      [...versionDelInfo.versionNames, { versionId: -1, name: '无' }].map((item) => (
                        <Option value={item.versionId}>{item.name}</Option>
                      ))
                    }
                  </Select>
                </div>
              </div>
            ) : ''
          }
        </div>
      </Modal>
    );
  }
}

export default DeleteReleaseWithIssue;
