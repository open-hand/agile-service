import React from 'react';
import { Button } from 'choerodon-ui/pro';
import DetailContainer, { useDetail } from '@/components/detail-container';

const PreviewIssue = () => {
  const [detailProps] = useDetail();
  const { open, close } = detailProps;
  return (
    <div>
      <Button onClick={() => {
        open({
          path: 'issue',
          props: {
            issueId: '=vRtVH0B2FJbfwRq_tETmtq1BG8IDJdEudoUfv610F3w==',
          },
          events: {

          },
        });
      }}
      >
        aa
      </Button>
      <DetailContainer {...detailProps} />
    </div>
  );
};
export default PreviewIssue;
