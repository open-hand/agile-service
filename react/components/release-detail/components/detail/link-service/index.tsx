import CustomIcon from '@/components/custom-icon';
import { Button, Icon, Tooltip } from 'choerodon-ui/pro/lib';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import React, { useMemo } from 'react';
import { useReleaseDetailContext } from '../../../stores';
import { openImportPomModal } from '../../import-pom';
import Section from '../../section';
import { openLinkServiceModal } from '../link-service-modal';
import './index.less';

const LinkService: React.FC = () => {
  const { disabled, prefixCls } = useReleaseDetailContext();
  const data = useMemo(() => [
    { type: 'service', name: '1.2222' },
    { type: 'pom', name: '1.2222' },

  ], []);
  return (
    <Section
      title="关联应用版本"
      buttons={
        !disabled ? (
          <div className={`${prefixCls}-link-service-operation`}>
            <Tooltip placement="topRight" autoAdjustOverflow={false} title="导入pom文件">
              <Button
                style={{ padding: '0 6px' }}
                color={'blue' as ButtonColor}
                //   icon="mode_edit"
                onClick={() => {
                  openImportPomModal();
                }}
              >
                <CustomIcon type="icon-pom" width={18} height={18} />
                {/* {svg} */}
              </Button>
            </Tooltip>
            <Tooltip placement="topRight" autoAdjustOverflow={false} title="关联应用版本">
              <Button
                style={{ padding: '0 6px' }}
                color={'blue' as ButtonColor}
                icon="local_offer"
                onClick={() => {
                  openLinkServiceModal();
                }}
              />
            </Tooltip>
          </div>
        ) : ''
      }
      contentClassName={`${prefixCls}-link-service`}
    >
      {data.map((item) => (
        <div className={`${prefixCls}-link-service-item`}>
          <span className={`${prefixCls}-link-service-item-left`}>
            {item.type === 'service' ? <Icon type="local_offer" style={{ fontSize: 15 }} /> : <CustomIcon type="icon-pom-multiColor" width={17} height={17} />}
            <span className={`${prefixCls}-link-service-item-left-text`}>{item.name}</span>
          </span>
          <Button icon="delete_forever" className={`${prefixCls}-link-service-item-btn`} />
        </div>
      ))}
    </Section>
  );
};
export default LinkService;
