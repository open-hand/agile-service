import React, {
  useCallback,
  useEffect, useMemo, useState,
} from 'react';
import {
  DataSet, Form, Modal, TextField,
} from 'choerodon-ui/pro';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { IModalProps } from '@/common/types';
import SelectAppService from '@/components/select/select-app-service';
import SelectGitTags from '@/components/select/select-git-tags';
import { getProjectId } from '@/utils/common';

interface ILinkServiceProps {
  handleOk?: ((data: any) => void) | (() => Promise<any>)
  publishVersionId: string
}

const LinkAppServiceTagModal: React.FC<{ modal?: IModalProps } & ILinkServiceProps> = ({
  modal, handleOk, publishVersionId,
}) => {
  const [applicationId, setApplicationId] = useState<string>();

  const ds = useMemo(() => new DataSet({
    autoQuery: false,
    autoCreate: true,
    paging: false,

    fields: [
      {
        name: 'appService', label: '选择应用服务', type: 'object' as any, required: true, ignore: 'always' as any,
      },
      { name: 'tagName', label: '选择tag', required: true },
      { name: 'alias', label: '版本别名', maxLength: 16 },
      { name: 'appServiceCode', bind: 'appService.code' },
      { name: 'projectId', defaultValue: getProjectId() },

    ],
  }), []);

  const handleSubmit = useCallback(async () => {
    if (!await ds.current?.validate()) {
      return false;
    }
    const data = ds?.toJSONData();
    const result = handleOk && await handleOk(data);
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
};
function openLinkAppServiceTagModal(props: ILinkServiceProps) {
  const key = Modal.key();
  Modal.open({
    key,
    title: '关联Tag',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <LinkAppServiceTagModal {...props} />,

  });
}
export { openLinkAppServiceTagModal };
