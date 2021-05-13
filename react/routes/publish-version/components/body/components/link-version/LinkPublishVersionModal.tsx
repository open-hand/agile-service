import React, {
  useCallback,
  useEffect, useMemo, useState,
} from 'react';
import {
  DataSet, Form, Modal, Select,
} from 'choerodon-ui/pro/lib';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
// import './index.less';
import { IModalProps } from '@/common/types';
import { publishVersionApiConfig } from '@/api';
// @ts-ignore
import JSONbig from 'json-bigint';

const JSONbigString = JSONbig({ storeAsString: true });

interface ILinkServiceProps {
  handleOk?: ((data: any) => void) | (() => Promise<any>)
  publishVersionId: string
}
const { Option } = Select;

const LinkPublishVersionModal: React.FC<{ modal?: IModalProps } & ILinkServiceProps> = ({
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
      // { name: 'appService', label: '选择应用服务', required: !programMode },
      // { name: 'subProject', label: '选择子项目', required: !!programMode },

      {
        name: 'version',
        label: '选择发布版本',
        multiple: true,
        textField: 'name',
        valueField: 'id',
        options: new DataSet({
          autoQuery: true,
          paging: false,
          transport: {
            read: {
              ...publishVersionApiConfig.loadDependencyTreeAvailableNode(publishVersionId),
              transformResponse: (res: any) => {
                const data = JSONbigString.parse(res);
                return data.map((item: any) => ({ ...item, name: item.serviceCode ? `${item.serviceCode}:${item.versionAlias || item.version}` : item.versionAlias || item.version }));
              },
            },
          },
        }),
      },
    ],
  }), [publishVersionId]);
  useEffect(() => {
    ds.current?.init(versionType, undefined);
  }, [ds, versionType]);
  const handleSubmit = useCallback(async () => {
    if (!await ds.current?.validate()) {
      return false;
    }
    const data = ds.current?.toData();
    const result = handleOk && await handleOk(data);
    return typeof (result) !== 'undefined' ? result : true;
  }, [ds, handleOk]);
  useEffect(() => {
    modal?.handleOk(handleSubmit);
  }, [handleSubmit, modal]);
  return (
    <Form dataSet={ds}>
      <Select name="version" />
    </Form>
  );
};
function openLinkPublishVersionModal(props: ILinkServiceProps) {
  const key = Modal.key();
  Modal.open({
    key,
    title: '关联发布版本',
    style: {
      width: MODAL_WIDTH.small,
    },
    drawer: true,
    children: <LinkPublishVersionModal {...props} />,

  });
}
export { openLinkPublishVersionModal };
