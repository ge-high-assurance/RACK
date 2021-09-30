//@ts-check

'use strict'

import * as path from 'path'

// import * as CopyWebpackPlugin from 'copy-webpack-plugin'
import HtmlWebpackPlugin from 'html-webpack-plugin'
import { Configuration } from 'webpack'
import 'webpack-dev-server'

const config: Configuration = {

    devServer: {
        hot: true,
        static: {
            directory: path.join(__dirname, 'dist'),
        },
    },

    devtool: 'inline-source-map',

    entry: './index.ts',

    mode: 'development',

    module: {
        rules: [

            {
                test: /\.tsx?$/,
                exclude: /node_modules/,
                use: 'ts-loader',
            },

            // {
            //     test: /\.json$/,
            //     exclude: /node_modules/,
            //     use: {
            //         loader: 'json-loader',
            //     },
            // },

        ],
    },

    output: {
        // devtoolModuleFilenameTemplate: '../[resource-path]',
        // filename: '[name].bundle.js',
        // library: {
        //     type: 'commonjs2',
        // },
        publicPath: '/',
        path: path.resolve(__dirname, 'dist'),
    },

    plugins: [
        // new CopyWebpackPlugin({
        //     patterns: [
        //         { from: 'static', to: 'static' },
        //     ],
        // }),
        new HtmlWebpackPlugin({ template: 'index.html' }),
    ],

    resolve: {
        extensions: ['.ts', '.tsx', '.js'],
    },

    target: 'web',

}

module.exports = config
