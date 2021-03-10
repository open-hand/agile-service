import CustomIcon from '@/components/custom-icon';
import { Button, Icon, Tooltip } from 'choerodon-ui/pro/lib';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import React from 'react';
import { useReleaseDetailContext } from '../../../stores';
import { openImportPomModal } from '../../import-pom';
import Section from '../../section';

const LinkService:React.FC = () => {
  const { disabled } = useReleaseDetailContext();
  return (
    <Section
      title="关联应用版本"
      buttons={
        !disabled ? (
          <Tooltip placement="topRight" autoAdjustOverflow={false} title="导入pom文件">
            <Button
              style={{ padding: '0 6px' }}
              color={'blue' as ButtonColor}
            //   icon="mode_edit"
              onClick={() => {
                openImportPomModal();
              }}
            >
              <CustomIcon type="icon-pom" width={16} height={16} />
              {/* {svg} */}
            </Button>
          </Tooltip>
        ) : ''
    }
    >
      关联应用版本
    </Section>
  );
};
export default LinkService;
