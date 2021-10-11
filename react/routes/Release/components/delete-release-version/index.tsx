import React, {
  useCallback, useEffect, useMemo, useState,
} from 'react';
import {
  DataSet, DatePicker, Form, Modal, TextField, Select, Icon,
} from 'choerodon-ui/pro';
import { FieldTrim } from 'choerodon-ui/pro/lib/data-set/enum';
import moment from 'moment';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { IModalProps } from '@/common/types';
import { versionApi } from '@/api';
import { getProjectId } from '@/utils/common';

interface PublishVersionModalProps {
    handleOk?: ((data: any) => void) | (() => Promise<any>)
}
interface PublishVersionModalWithEditProps extends PublishVersionModalProps {
    versionDelInfo: any
}
const { Option } = Select;
const DeletePublishVersion: React.FC<{ modal?: IModalProps } & PublishVersionModalWithEditProps> = ({
  modal, handleOk, versionDelInfo,
}) => {
  const [distributed, setDistributed] = useState<boolean>(!!versionDelInfo.versionNames[0]?.versionId);
  const [targetVersionId, setTargetVersionId] = useState<string | null | undefined>(versionDelInfo.versionNames[0]?.versionId);
  const handleSubmit = useCallback(async () => {
    const result = await versionApi.delete(versionDelInfo.versionId,
      (versionDelInfo.agileIssueCount && distributed && targetVersionId) ? targetVersionId : undefined).then((data: any) => {
      handleOk && handleOk(versionDelInfo);
    }).catch(() => {
    });
    return typeof (result) !== 'undefined' ? result : true;
  }, [distributed, handleOk, targetVersionId, versionDelInfo]);
  useEffect(() => {
        modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  return (
    <>
      <p style={{ marginTop: 0, marginBottom: 0 }}>
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
                          此版本有
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
                          。
                          {Object.keys(versionDelInfo).length && versionDelInfo.versionNames.length && versionDelInfo.agileIssueCount ? '相关的工作项将移动到下面选择的版本中。' : ''}
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
                        <div style={{ flex: 4, marginBottom: -20 }}>
                          <Form>
                            <Select
                              style={{
                                width: '100%',
                              }}
                              label="请选择要移动到版本"
                              onChange={(value) => {
                                setTargetVersionId(value);
                                setDistributed(value !== -1);
                              }}
                              defaultValue={versionDelInfo.versionNames[0]?.versionId}
                            >
                              {
                                            [...versionDelInfo.versionNames, { versionId: -1, name: '无' }].map((item) => (
                                              <Option value={item.versionId}>{item.name}</Option>
                                            ))
                                        }
                            </Select>
                          </Form>
                        </div>
                      </div>
                    ) : ''
                }
      </div>
    </>
  );
};
function openDeleteReleaseVersionModal(props: PublishVersionModalWithEditProps) {
  const key = Modal.key();
  Modal.open({
    key,
    title: '删除版本',

    children: <DeletePublishVersion {...props} />,
    okText: '删除',
  });
}
export default openDeleteReleaseVersionModal;
