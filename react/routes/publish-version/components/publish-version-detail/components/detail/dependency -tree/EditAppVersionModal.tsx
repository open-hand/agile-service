import React, {
  useMemo, useCallback, useEffect, useState,
} from 'react';
import {
  DataSet, Form, Modal, Table, TextField,
} from 'choerodon-ui/pro/lib';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import './index.less';
import { IModalProps } from '@/common/types';
import { IAppVersionData, publishVersionApi, versionApiConfig } from '@/api';
import { observer } from 'mobx-react-lite';
import SelectAppService from '@/components/select/select-app-service';
import SelectGitTags from '@/components/select/select-git-tags';
import IPublishVersionDetailData from '../../../types';

interface IImportPomFunctionProps {
  handleOk?: ((data: any) => void) | (() => Promise<any>)
  data?: IPublishVersionDetailData
}

const EditAppVersionModal: React.FC<{ modal?: IModalProps } & Partial<IImportPomFunctionProps>> = ({ modal, handleOk, data }) => {
  const [applicationId, setApplicationId] = useState<string>();
  const formDs = useMemo(() => new DataSet({
    autoCreate: true,
    data: data ? [data] : undefined,
    fields: [
      { name: 'versionAlias', label: '版本别名' },
      { name: 'version', label: 'version' },
      { name: 'artifactId', label: 'artifactId' },
      { name: 'groupId', label: 'groupId' },
      { name: 'serviceCode', label: '关联应用服务' },
      { name: 'tag', label: '关联tag' },
    ],
    transport: {
      submit: data ? ({ data: newData }) => publishVersionApi.update(data.id!, newData[0]) : undefined,
    },
  }), [data]);

  const handleSubmit = useCallback(async () => {
    if (!data && !await formDs.validate()) {
      return false;
    }
    data && await formDs.submit();
    const result = handleOk && await handleOk(formDs.current?.toData());
    return typeof (result) !== 'undefined' ? result : true;
  }, [data, formDs, handleOk]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  return (
    <Form dataSet={formDs}>
      <TextField name="versionAlias" />

      <TextField name="artifactId" />

      <TextField name="version" />

      <SelectAppService name="serviceCode" onChange={setApplicationId} />
      <SelectGitTags name="tag" applicationId={applicationId} />
    </Form>
  );
};
const ObserverEditAppVersionModal = observer(EditAppVersionModal);
function openEditAppVersionModal(props: IImportPomFunctionProps) {
  const key = Modal.key();
  Modal.open({
    key,
    title: '修改应用版本',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <ObserverEditAppVersionModal {...props} />,
  });
}
function openCreateAppVersionModal(props: IImportPomFunctionProps) {
  const key = Modal.key();
  Modal.open({
    key,
    title: '创建应用版本',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <ObserverEditAppVersionModal {...props} />,
  });
}
export { openEditAppVersionModal, openCreateAppVersionModal };
