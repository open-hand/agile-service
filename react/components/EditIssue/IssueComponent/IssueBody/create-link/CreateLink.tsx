import React, {
  useEffect, useCallback, useState, useRef,
  useMemo,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Modal, Form, Select, DataSet,
} from 'choerodon-ui/pro';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { Choerodon } from '@choerodon/boot';
import { map, find } from 'lodash';
import MODAL_WIDTH from '@/constants/MODAL_WIDTH';
import { IModalProps } from '@/common/types';
import { IIssueLink, issueLinkApi, issueLinkTypeApi } from '@/api';
import LinkedTable from './LinkedTable';

const { Option } = Select;
interface IOriginLink {
  linkTypeId: string,
  inWard: string,
  outWard: string,
  linkName: string
}

interface ILink {
  name: string,
  linkTypeId: string
}
interface Props {
  modal?: IModalProps,
  issueId: string,
  onOk: (res: any) => void,
}
const CreateLink: React.FC<Props> = ({ modal, issueId, onOk }) => {
  const [originLinks, setOriginLinks] = useState<IOriginLink[]>([]);
  const [showLinks, setShowLinks] = useState<ILink[]>([]);
  const linkedTableRef = useRef<{
    linkedIssues: string[] | undefined
  } | undefined>();

  const transform = useCallback((links: IOriginLink[]) => {
    const active = links.map((link) => ({
      name: link.outWard,
      linkTypeId: link.linkTypeId,
    }));
    const passive: ILink[] = [];
    links.forEach((link) => {
      if (link.inWard !== link.outWard) {
        passive.push({
          name: link.inWard,
          linkTypeId: link.linkTypeId,
        });
      }
    });
    setShowLinks(active.concat(passive));
  }, []);

  const getLinks = useCallback(() => {
    issueLinkTypeApi.getAll().then((res: { list: IOriginLink[] }) => {
      setOriginLinks(res.list);
      transform(res.list);
    });
  }, [transform]);

  useEffect(() => {
    getLinks();
  }, [getLinks]);

  const linkedDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'linkType',
      type: 'string' as FieldType,
      label: '关系',
      textField: 'name',
      valueField: 'linkTypeId',
      required: true,
    }],
  }), []);

  const handleOk = useCallback(async () => {
    const validate = await linkedDataSet.validate();
    if (validate) {
      const linkTypeId: string = linkedDataSet?.current?.get('linkType');
      const selected = linkedTableRef.current?.linkedIssues || [];
      if (!selected.length) {
        Choerodon.prompt('请选择要关联的工作项');
        return false;
      }
      const labelIssueRelVOList: IIssueLink[] = map(selected, (issue) => {
        const currentLinkType = find(originLinks, { linkTypeId: linkTypeId.split('+')[0] });
        if (currentLinkType && currentLinkType.outWard === linkTypeId.split('+')[1]) {
          return ({
            linkTypeId: linkTypeId.split('+')[0],
            linkedIssueId: issue,
            issueId,
          });
        }
        return ({
          linkTypeId: linkTypeId.split('+')[0],
          issueId: issue,
          linkedIssueId: issueId,
        });
      });
      return issueLinkApi.create(issueId, labelIssueRelVOList)
        .then((r: any) => {
          onOk(r);
          return true;
        });
    }
    return false;
  }, [issueId, linkedDataSet, onOk, originLinks]);

  useEffect(() => {
    modal?.handleOk(handleOk);
  }, [handleOk, modal]);

  return (
    <>
      <Form style={{ paddingRight: 5 }}>
        <Select
          dataSet={linkedDataSet}
          name="linkType"
          label="关系"
        >
          {showLinks.map((link) => (
            <Option key={`${link.linkTypeId}+${link.name}`} value={`${link.linkTypeId}+${link.name}`}>
              {link.name}
            </Option>
          ))}
        </Select>
      </Form>
      <LinkedTable issueId={issueId} linkedTableRef={linkedTableRef} />
    </>
  );
};

const ObserverCreateLink = observer(CreateLink);

const openCreateLink = (props: Props) => {
  Modal.open({
    key: Modal.key(),
    title: '创建工作项链接',
    drawer: true,
    style: {
      width: MODAL_WIDTH.middle + 100,
    },
    children: <ObserverCreateLink {...props} />,
  });
};

export default openCreateLink;
