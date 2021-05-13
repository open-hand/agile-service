import React, {
  useCallback,
  useEffect, useMemo, useState,
} from 'react';
import {
  DataSet, Form, Modal, Select, TextField,
} from 'choerodon-ui/pro';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
// import './index.less';
import { IModalProps } from '@/common/types';
import { publishVersionApiConfig } from '@/api';
// @ts-ignore
import JSONbig from 'json-bigint';
import SelectAppService from '@/components/select/select-app-service';
import SelectGitTags from '@/components/select/select-git-tags';
import { getProjectId } from '@/utils/common';

const JSONbigString = JSONbig({ storeAsString: true });

interface ILinkServiceProps {
  handleOk?: ((data: any) => void) | (() => Promise<any>)
  publishVersionId: string
}
const { Option } = Select;

const LinkAppServiceTagModal: React.FC<{ modal?: IModalProps } & ILinkServiceProps> = ({
  modal, handleOk, publishVersionId,
}) => {
  const [applicationId, setApplicationId] = useState<string>();
  const [versionType, setVersionType] = useState<string>('version');

  const ds = useMemo(() => new DataSet({
    autoQuery: false,
    autoCreate: true,
    paging: false,
    // data: [
    //   { appService: '应用1', alias: undefined },
    // ],
    fields: [
      {
        name: 'appService', label: '选择应用服务', type: 'object' as any, required: true, ignore: 'always' as any,
      },
      { name: 'tagName', label: '选择tag', required: true },
      { name: 'versionAlias', label: '版本别名', maxLength: 16 },
      { name: 'appServiceCode', bind: 'appService.code' },
      { name: 'projectId', defaultValue: getProjectId() },
      // { name: 'subProject', label: '选择子项目', required: !!programMode },

    ],
  }), []);
  useEffect(() => {
    ds.current?.init(versionType, undefined);
  }, [ds, versionType]);
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
      <TextField name="versionAlias" />

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
