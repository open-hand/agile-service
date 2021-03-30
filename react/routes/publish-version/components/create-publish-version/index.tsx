import React, {
  useCallback,
  useEffect, useMemo, useRef, useState,
} from 'react';
import {
  Button,
  DataSet, DatePicker, Form, Modal, Radio, Select, TextField, Tooltip,
} from 'choerodon-ui/pro/lib';
import { debounce, isEmpty } from 'lodash';
import classnames from 'classnames';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import SelectAppService from '@/components/select/select-app-service';
import SelectGitTags from '@/components/select/select-git-tags';
import RadioGroup from 'choerodon-ui/lib/radio/group';
// import './index.less';
import { SelectProps } from 'choerodon-ui/pro/lib/select/Select';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { observer } from 'mobx-react-lite';
import { IModalProps } from '@/common/types';
import {
  IAppVersionData, publishVersionApi, publishVersionApiConfig, versionApi,
} from '@/api';
import { Checkbox } from 'choerodon-ui';
import SelectTeam from '@/components/select/select-team';

interface ILinkServiceProps {
  handleOk?: ((data: any) => void) | (() => Promise<any>)

}
const { Option } = Select;

const CreatePublishVersion: React.FC<{ modal?: IModalProps } & ILinkServiceProps> = ({
  modal, handleOk,
}) => {
  const [applicationId, setApplicationId] = useState<string>();
  const [versionType, setVersionType] = useState<string>('version');
  function handleCheckName(newName: string) {
    return publishVersionApi.checkAlias(newName).then((res: boolean) => (res ? '版本名称重复' : true));
  }
  const ds = useMemo(() => new DataSet({
    autoQuery: false,
    autoCreate: true,
    paging: false,
    // data: [
    //   { appService: '应用1', alias: undefined },
    // ],
    fields: [
      {
        name: 'versionAlias', label: '发布版本名称', required: true, validator: handleCheckName,
      },
      { name: 'actualPublishDate', label: '实际发布时间' },
      { name: 'artifactId', label: 'artifactId' },
      { name: 'groupId', label: 'groupId' },
      { name: 'appService', label: '关联应用服务', defaultValue: true },
      { name: 'serviceCode', label: '关联应用服务' },

      { name: 'tagId', label: '关联tag' },
    ],
    transport: {
      submit: ({ data }) => publishVersionApiConfig.create(data[0]),
    },
  }), []);
  useEffect(() => {
    ds.current?.init(versionType, undefined);
  }, [ds, versionType]);
  const handleSubmit = useCallback(async () => {
    if (!await ds.current?.validate()) {
      return false;
    }
    const data = ds.current?.toData();
    await ds.submit();
    const result = handleOk && await handleOk(data);
    return typeof (result) !== 'undefined' ? result : true;
  }, [ds, handleOk]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  return (
    <Form dataSet={ds}>
      <TextField name="versionAlias" />
      <DatePicker name="actualPublishDate" />
      <TextField name="artifactId" />
      <TextField name="groupId" />
      <SelectAppService name="serviceCode" onChange={setApplicationId} />
      <SelectGitTags name="tagId" applicationId={applicationId} />
    </Form>
  );
};
function openCreatePublishVersionModal(props: ILinkServiceProps) {
  const key = Modal.key();
  Modal.open({
    key,
    title: '创建发布版本',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <CreatePublishVersion {...props} />,

  });
}
export { openCreatePublishVersionModal };
