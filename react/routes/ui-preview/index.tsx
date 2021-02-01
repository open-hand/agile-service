import React from 'react';
import { useParams } from 'react-router-dom';
// @ts-ignore
import { API_HOST } from '@choerodon/master/lib/utils/constants';

const UiPreview = () => {
  const { uuid } = useParams<{ uuid: string }>();
  return (
    <div>
      <iframe
        title="preview"
        src={`${API_HOST}/agile/v1/static_file/resource/${uuid}/index`}
        frameBorder="0"
        height="100%"
        width="100%"
      />
    </div>
  );
};

export default UiPreview;
