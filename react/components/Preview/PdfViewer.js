import React from 'react';

const Pdf = ({ file }) => (
  <embed
    src={file}
    style={{
      width: '100%',
      height: '100%',
    }}
  />
);

export default Pdf;
