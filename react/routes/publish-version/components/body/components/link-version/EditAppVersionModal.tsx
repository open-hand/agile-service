import React, {
  useMemo, useCallback, useEffect, useState,
} from 'react';
import {
  DataSet, Form, Modal, TextField,
} from 'choerodon-ui/pro';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import './index.less';
import { IModalProps } from '@/common/types';
import { observer } from 'mobx-react-lite';
import SelectAppService from '@/components/select/select-app-service';
import SelectGitTags from '@/components/select/select-git-tags';

interface IImportPomFunctionProps {
  handleOk?: ((data: any) => void) | (() => Promise<any>)
  data: any
}

const EditAppVersionModal: React.FC<{ modal?: IModalProps } & Partial<IImportPomFunctionProps>> = ({ modal, handleOk, data }) => {
  const formDs = useMemo(() => new DataSet({
    autoCreate: true,
    data: data ? [data] : undefined,
    fields: [
      {
        name: 'tagAlias', label: '版本别名', required: false, maxLength: 16,
      },
      // { name: 'serviceCode', label: '关联应用服务' },
      // { name: 'tagName', label: '关联tag' },
    ],
  }), [data]);

  const handleSubmit = useCallback(async () => {
    if (!data && !await formDs.validate()) {
      return false;
    }
    const result = handleOk && await handleOk(formDs.current?.toData());
    return typeof (result) !== 'undefined' ? result : true;
  }, [data, formDs, handleOk]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);

  return (
    <Form dataSet={formDs}>
      <TextField name="tagAlias" />
    </Form>
  );
};
const ObserverEditAppVersionModal = observer(EditAppVersionModal);
const EditLinkAppServiceModal: React.FC<{ modal?: IModalProps } & IImportPomFunctionProps> = observer(({
  modal, handleOk, data,
}) => {
  const [applicationId, setApplicationId] = useState<string>();

  const ds = useMemo(() => new DataSet({
    autoQuery: false,
    autoCreate: true,
    paging: false,
    data: [data],
    fields: [
      { name: 'artifactId', label: 'Artifact' },
      { name: 'version', label: 'Version', required: true },
      {
        name: 'appService', label: '选择应用服务', type: 'object' as any, required: true, ignore: 'always' as any,
      },
      { name: 'tagName', label: '选择tag', required: true },
      { name: 'alias', label: '版本别名', maxLength: 16 },
      { name: 'appServiceCode', bind: 'appService.code' },

    ],
  }), []);

  const handleSubmit = useCallback(async () => {
    if (!await ds.current?.validate()) {
      return false;
    }
    const newData = ds?.toJSONData();
    const result = handleOk && await handleOk(newData);
    return typeof (result) !== 'undefined' ? result : true;
  }, [ds, handleOk]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  return (
    <Form dataSet={ds}>
      <SelectAppService
        name="appService"
        onChange={(v) => {
          setApplicationId(v ? v.id : undefined);
        }}
      />
      <SelectGitTags key={`select-git-tag-${applicationId}`} name="tagName" applicationId={applicationId} />
      <TextField name="alias" />

    </Form>
  );
});
function openEditLinkAppServiceModal(props: IImportPomFunctionProps) {
  const key = Modal.key();
  Modal.open({
    key,
    title: '修改关联版本',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <EditLinkAppServiceModal {...props} />,
  });
}
async function openEditAppVersionModal(props: IImportPomFunctionProps) {
  const key = Modal.key();
  Modal.open({
    key,
    title: '修改关联版本别名',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <ObserverEditAppVersionModal {...props} />,
  });
}

export { openEditAppVersionModal, openEditLinkAppServiceModal };
